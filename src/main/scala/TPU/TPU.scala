package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

case class TPUParams (inputWidth: Int, outputWidth: Int, m: Int, k: Int, n: Int) {
    // TODO: parametrize matrix size.
    // A (m x k) X B (k x n) = C (m x n)
    val iw: Int = inputWidth // Input bitwidth
    val ow: Int = outputWidth
    

    val sysw: Int = m // TODO: fix when we remove last sysw

    val aRows: Int = m
    val aCols: Int = k

    val bRows: Int = k
    val bCols: Int = n

    val cRows: Int = m
    val cCols: Int = n
}

class TPU_fixed(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        val inTopNE = Flipped(Decoupled(SInt(p.iw.W)))
        val inTopNW = Flipped(Decoupled(SInt(p.iw.W)))
        val inLeftNW = Flipped(Decoupled(SInt(p.iw.W)))
        val inLeftSW = Flipped(Decoupled(SInt(p.iw.W)))

        val clear = Input(Bool())

        val outNE = Decoupled(SInt(p.ow.W))
        val outNW = Decoupled(SInt(p.ow.W))
        val outSE = Decoupled(SInt(p.ow.W))
        val outSW = Decoupled(SInt(p.ow.W))
    })
    // TODO: Parameterize generation of MACS, hardcode for now
    val macNE = Module(new MAC(MACParams(p.iw, p.ow))) // inLeft, inTop, outRight, outBottom, outResult
    val macNW = Module(new MAC(MACParams(p.iw, p.ow)))
    val macSE = Module(new MAC(MACParams(p.iw, p.ow)))
    val macSW = Module(new MAC(MACParams(p.iw, p.ow)))


    // ----------------------------------------------- Systolic Array IO -----------------------------------------------
    // IO Data and Valid to Systolic Array Input
    macNE.io.inTop.bits := io.inTopNE.bits
    macNW.io.inTop.bits := io.inTopNW.bits
    macNW.io.inLeft.bits := io.inLeftNW.bits
    macSW.io.inLeft.bits := io.inLeftSW.bits

    macNE.io.inTop.valid   := io.inTopNE.valid
    macNW.io.inTop.valid   := io.inTopNW.valid
    macNW.io.inLeft.valid  := io.inLeftNW.valid
    macSW.io.inLeft.valid  := io.inLeftSW.valid

    // IO Data and Valid from Systolic Array Output
    io.outNE.valid := macNE.io.outResult.valid
    io.outNW.valid := macNW.io.outResult.valid
    io.outSE.valid := macSE.io.outResult.valid
    io.outSW.valid := macSW.io.outResult.valid

    io.outNE.bits := macNE.io.outResult.bits
    io.outNW.bits := macNW.io.outResult.bits
    io.outSE.bits := macSE.io.outResult.bits
    io.outSW.bits := macSW.io.outResult.bits

    // IO ready to Systolic Array Output
    macNE.io.outResult.ready := io.outNE.ready
    macNW.io.outResult.ready := io.outNW.ready
    macSE.io.outResult.ready := io.outSE.ready
    macSW.io.outResult.ready := io.outSW.ready

    // Clear Inputs for Systolic Array
    macNW.io.clear := io.clear
    macNE.io.clear := io.clear
    macSE.io.clear := io.clear
    macSW.io.clear := io.clear

    // ----------------------------------------------- Systolic Array Internal Wiring -----------------------------------------------
    // Ready-Valid connections between MACs
    macNW.io.outRight.ready  := macNE.io.inLeft.ready
    macNW.io.outBottom.ready := macSW.io.inTop.ready
    macSW.io.outRight.ready  := macSE.io.inLeft.ready
    macNE.io.outBottom.ready := macSE.io.inTop.ready

    macNE.io.inLeft.valid := macNW.io.outRight.valid
    macSE.io.inLeft.valid := macSW.io.outRight.valid    
    macSW.io.inTop.valid := macNW.io.outBottom.valid
    macSE.io.inTop.valid := macNE.io.outBottom.valid
    
    // Data Connections between MACS
    macNE.io.inLeft.bits := macNW.io.outRight.bits
    macSE.io.inLeft.bits := macSW.io.outRight.bits
    macSW.io.inTop.bits := macNW.io.outBottom.bits
    macSE.io.inTop.bits := macNE.io.outBottom.bits

    // Defaults
    // east edge drains to nowhere
    macNE.io.outRight.ready   := true.B   
    macSE.io.outRight.ready   := true.B
    // south edge drains to nowhere
    macSW.io.outBottom.ready  := true.B
    macSE.io.outBottom.ready  := true.B

    io.inTopNE.ready  := true.B
    io.inTopNW.ready  := true.B
    io.inLeftNW.ready := true.B
    io.inLeftSW.ready := true.B        
}


class Top_vec_io(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        
        val clear = Input(Bool())
        // Vector input and output matrices
        val in = Flipped(Decoupled(new Bundle {
            val a = Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W)))
            val b = Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W)))
        }))
        val out = Valid(Vec(p.aRows, Vec(p.bCols, SInt(p.ow.W))))

    })
    // 2x2 fixed Systolic Array
    val TPU = Module(new TPU_fixed(p))

    // Matrix Storage
    val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W))))
    val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W))))
    // Initialize output mat to zeros
    val cReg = RegInit(VecInit.tabulate(p.aRows)(_ =>
                VecInit.fill(p.bCols)(0.S(p.ow.W))))

    // -1 is the idle state
    val writeCycle = new Counter(6)    // holds -1,0,1,2,3,4

    // Registers representing times to read from MACs
    val nwReadsRemaining = RegInit(0.U(2.W))
    val neReadsRemaining = RegInit(0.U(2.W))
    val swReadsRemaining = RegInit(0.U(2.W))
    val seReadsRemaining = RegInit(0.U(2.W))

    // When register is zero, MAC value is finalized
    TPU.io.outNW.ready := nwReadsRemaining =/= 0.U
    TPU.io.outNE.ready := neReadsRemaining =/= 0.U
    TPU.io.outSW.ready := swReadsRemaining =/= 0.U
    TPU.io.outSE.ready := seReadsRemaining =/= 0.U

    // When outresult is valid and nonzero and it can still be read, read it out
    when (TPU.io.outNW.fire) { cReg(0)(0) := TPU.io.outNW.bits ; nwReadsRemaining := nwReadsRemaining - 1.U }
    when (TPU.io.outNE.fire) { cReg(0)(1) := TPU.io.outNE.bits ; neReadsRemaining := neReadsRemaining - 1.U }
    when (TPU.io.outSW.fire) { cReg(1)(0) := TPU.io.outSW.bits ; swReadsRemaining := swReadsRemaining - 1.U }
    when (TPU.io.outSE.fire) { cReg(1)(1) := TPU.io.outSE.bits ; seReadsRemaining := seReadsRemaining - 1.U }
    
    // Defaults

    // IO Data and Valid to Systolic Array Input
    TPU.io.inTopNE.valid := false.B
    TPU.io.inTopNW.valid := false.B
    TPU.io.inLeftNW.valid := false.B
    TPU.io.inLeftSW.valid := false.B

    TPU.io.inTopNE.bits := 0.S
    TPU.io.inTopNW.bits := 0.S
    TPU.io.inLeftNW.bits := 0.S
    TPU.io.inLeftSW.bits := 0.S

    io.out.bits  := cReg
    io.out.valid := false.B
    io.in.ready := true.B

    TPU.io.clear := io.clear

    switch(writeCycle.value) {
    // Flashes input matrices to local registers in state -1
        is(0.U){
            aReg := io.in.bits.a
            bReg := io.in.bits.b
            writeCycle.inc()  
        }
        is(1.U) {
            // First Write
            nwReadsRemaining := 2.U
            TPU.io.inTopNW.valid := true.B
            TPU.io.inLeftNW.valid := true.B

            TPU.io.inTopNW.bits := bReg(0)(0)
            TPU.io.inLeftNW.bits := aReg(0)(0)

            writeCycle.inc()  
        }
        is(2.U) {
            // Last write
            TPU.io.inTopNW.valid := true.B
            TPU.io.inLeftNW.valid := true.B
            TPU.io.inTopNW.bits := bReg(1)(0)
            TPU.io.inLeftNW.bits := aReg(0)(1)

            // First Write
            neReadsRemaining := 2.U
            swReadsRemaining := 2.U
            TPU.io.inTopNE.valid := true.B
            TPU.io.inLeftSW.valid := true.B
            TPU.io.inTopNE.bits := bReg(0)(1)
            TPU.io.inLeftSW.bits := aReg(1)(0)


            writeCycle.inc()  
        }
        is(3.U) {
            // Last write
            seReadsRemaining := 2.U
            TPU.io.inTopNE.valid := true.B
            TPU.io.inLeftSW.valid := true.B
            TPU.io.inTopNE.bits := bReg(1)(1)
            TPU.io.inLeftSW.bits := aReg(1)(1)

            writeCycle.inc()  
        }
        is(4.U) {
            when (neReadsRemaining === 0.U && swReadsRemaining === 0.U) {
                writeCycle.inc()  
            }
        }
        is(5.U) {
            // Last read
            when (nwReadsRemaining === 0.U && seReadsRemaining === 0.U) {
                writeCycle.inc()  
            }
        }
    }
    io.out.bits := cReg
    io.out.valid := writeCycle.value === 4.U && nwReadsRemaining === 0.U && neReadsRemaining  === 0.U && swReadsRemaining  === 0.U && seReadsRemaining  === 0.U

    TPU.io.clear := io.clear
}

class TPU_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        val clear = Input(Bool())

        // Row Major Order ([NW, NE], [SW, SE])
        val inTop  = Flipped(Vec(p.aCols,  Decoupled(SInt(p.iw.W))))
        val inLeft = Flipped(Vec(p.aRows,  Decoupled(SInt(p.iw.W))))

        val out = Vec(p.aRows, Vec(p.bCols, Decoupled(SInt(p.ow.W))))
    })

    // TODO: Parameterize generation of MACS, hardcode for now
    val systolicArray: Seq[Seq[MAC]] =
        Seq.fill(p.sysw, p.sysw)(Module(new MAC(MACParams(p.iw, p.ow))))

    // ----------------------------------------------- Systolic Array IO -----------------------------------------------
    // IO Data and Valid to Systolic Array Input

    // Connect data and valid inputs to Systolic Array
    for (i <- 0 until p.sysw) {
        systolicArray(0)(i).io.inTop.valid := io.inTop(i).valid
        systolicArray(0)(i).io.inTop.bits := io.inTop(i).bits
        io.inTop(i).ready := true.B

        systolicArray(i)(0).io.inLeft.valid := io.inLeft(i).valid
        systolicArray(i)(0).io.inLeft.bits := io.inLeft(i).bits
        io.inLeft(i).ready := true.B
    }


    // Connect outResult outputs from systolic array
    for (row <- 0 until p.cRows) {
        for(col <- 0 until p.cCols) {
            // IO Bits and Valid
            io.out(row)(col).bits := systolicArray(row)(col).io.outResult.bits
            io.out(row)(col).valid := systolicArray(row)(col).io.outResult.valid
            // IO Ready 
            
            systolicArray(row)(col).io.outResult.ready := io.out(row)(col).ready
            // Clear
            systolicArray(row)(col).io.clear := io.clear
        }
    }

    // ----------------------------------------------- Systolic Array Internal Wiring -----------------------------------------------
    // Ready horizontal connections between MACs
    for(row <- 0 until p.cRows) {
        for(col <- 0 until p.cCols) {
            // first element ignore valid connection (goes to IO) 
            if (col > 0) {
                    systolicArray(row)(col - 1).io.outRight.ready := systolicArray(row)(col).io.inLeft.ready
                    systolicArray(row)(col).io.inLeft.valid := systolicArray(row)(col - 1).io.outRight.valid
                    systolicArray(row)(col).io.inLeft.bits := systolicArray(row)(col - 1).io.outRight.bits
            }
            if (col == p.cCols - 1) {
                // Defaults since drains to edge
                systolicArray(row)(col).io.outRight.ready := true.B
            }
        }
    }
    // Ready vertical connections between MACs
    for(col <- 0 until p.cRows) {
        for(row <- 0 until p.cCols) {
            if (row > 0) {
                systolicArray(row - 1)(col).io.outBottom.ready := systolicArray(row)(col).io.inTop.ready
                systolicArray(row)(col).io.inTop.valid := systolicArray(row - 1)(col).io.outBottom.valid
                systolicArray(row)(col).io.inTop.bits := systolicArray(row - 1)(col).io.outBottom.bits
            }
            if (row == p.cRows - 1) {
                // Defaults since drains to edge
                systolicArray(row)(col).io.outBottom.ready := true.B
            }
        }
    }
}

class Top_fixed_TPU_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        
        val clear = Input(Bool())
        // Vector input and output matrices
        val in = Flipped(Decoupled(new Bundle {
            val a = Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W)))
            val b = Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W)))
        }))
        val out = Valid(Vec(p.aRows, Vec(p.bCols, SInt(p.ow.W))))

    })
    // 2x2 fixed Systolic Array
    val TPU = Module(new TPU_parameterized(p))

    // Matrix Storage
    val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W))))
    val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W))))
    // Initialize output mat to zeros
    val cReg = RegInit(VecInit.tabulate(p.aRows)(_ =>
                VecInit.fill(p.bCols)(0.S(p.ow.W))))

    // -1 is the idle state
    val writeCycle = new Counter(6)    // holds -1,0,1,2,3,4

    // When register is zero, MAC value is finalized
    val prevNW = RegNext(TPU.io.out(0)(0).bits)
    val prevNE = RegNext(TPU.io.out(0)(1).bits)
    val prevSW = RegNext(TPU.io.out(1)(0).bits)
    val prevSE = RegNext(TPU.io.out(1)(1).bits)

    val NWChanged = TPU.io.out(0)(0).bits =/= prevNW
    val NEChanged = TPU.io.out(0)(1).bits =/= prevNE
    val SWChanged = TPU.io.out(1)(0).bits =/= prevSW
    val SEChanged = TPU.io.out(1)(1).bits =/= prevSE

    val nwReadsRemaining = RegInit(0.U(2.W))
    val neReadsRemaining = RegInit(0.U(2.W))
    val swReadsRemaining = RegInit(0.U(2.W))
    val seReadsRemaining = RegInit(0.U(2.W))

    // When outresult is valid and nonzero and it can still be read, read it out
    when (NWChanged && TPU.io.out(0)(0).valid) { cReg(0)(0) := TPU.io.out(0)(0).bits ; nwReadsRemaining := nwReadsRemaining - 1.U }
    when (NEChanged && TPU.io.out(0)(1).valid) { cReg(0)(1) := TPU.io.out(0)(1).bits ; neReadsRemaining := neReadsRemaining - 1.U }
    when (SWChanged && TPU.io.out(1)(0).valid) { cReg(1)(0) := TPU.io.out(1)(0).bits ; swReadsRemaining := swReadsRemaining - 1.U }
    when (SEChanged && TPU.io.out(1)(1).valid) { cReg(1)(1) := TPU.io.out(1)(1).bits ; seReadsRemaining := seReadsRemaining - 1.U }


    // When register is zero, MAC value is finalized
    TPU.io.out(0)(0).ready := nwReadsRemaining =/= 0.U
    TPU.io.out(0)(1).ready := neReadsRemaining =/= 0.U
    TPU.io.out(1)(0).ready := swReadsRemaining =/= 0.U
    TPU.io.out(1)(1).ready := seReadsRemaining =/= 0.U
    
    // Defaults

    // IO Data and Valid to Systolic Array Input
    TPU.io.inTop(0).valid := false.B
    TPU.io.inTop(1).valid := false.B
    TPU.io.inLeft(0).valid := false.B
    TPU.io.inLeft(1).valid := false.B

    TPU.io.inTop(0).bits := 0.S
    TPU.io.inTop(1).bits := 0.S
    TPU.io.inLeft(0).bits := 0.S
    TPU.io.inLeft(1).bits := 0.S

    io.out.bits  := cReg
    io.out.valid := false.B
    io.in.ready := true.B

    TPU.io.clear := io.clear

    switch(writeCycle.value) {
    // Flashes input matrices to local registers in state -1
        is(0.U){
            aReg := io.in.bits.a
            bReg := io.in.bits.b
            writeCycle.inc()  
        }
        is(1.U) {
            // First Write
            TPU.io.inLeft(0).valid := true.B
            TPU.io.inTop(0).valid := true.B

            TPU.io.inLeft(0).bits := aReg(0)(0)
            TPU.io.inTop(0).bits := bReg(0)(0)

            writeCycle.inc()  
        }
        is(2.U) {
            // Last write
            TPU.io.inLeft(0).valid := true.B
            TPU.io.inTop(0).valid := true.B
            TPU.io.inLeft(0).bits := aReg(0)(1)
            TPU.io.inTop(0).bits := bReg(1)(0)

            // First Write
            TPU.io.inLeft(1).valid := true.B
            TPU.io.inTop(1).valid := true.B
            TPU.io.inLeft(1).bits := aReg(1)(0)
            TPU.io.inTop(1).bits := bReg(0)(1)


            writeCycle.inc()  
        }
        is(3.U) {
            // Last write
            TPU.io.inLeft(1).valid := true.B
            TPU.io.inTop(1).valid := true.B
            TPU.io.inLeft(1).bits := aReg(1)(1)
            TPU.io.inTop(1).bits := bReg(1)(1)

            writeCycle.inc()  
        }
        is(4.U) {
            writeCycle.inc()  

        }
        is(5.U) {
            // Last read
            writeCycle.inc()  
        }
    }
    io.out.bits := cReg
    io.out.valid := writeCycle.value === 5.U 

    TPU.io.clear := io.clear
}

class Top_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        
        val clear = Input(Bool())
        // Vector input and output matrices
        val in = Flipped(Decoupled(new Bundle {
            val a = Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W)))
            val b = Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W)))
        }))
        val out = Valid(Vec(p.aRows, Vec(p.bCols, SInt(p.ow.W))))

    })
    // 2x2 parametrized systolic array
    val TPU = Module(new TPU_parameterized(p))

    // Matrix Storage
    val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W))))
    val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W))))
    // Initialize output mat to zeros
    val cReg = RegInit(VecInit.tabulate(p.aRows)(_ =>
                VecInit.fill(p.bCols)(0.S(p.ow.W))))

    // -1 is the idle state
    // TODO Elaborate counter length as a function of the systolic array size
    val writeCycle = new Counter(6)    // holds -1,0,1,2,3,4

    // Registers representing times to read from MACs

    // Stores previous values of each MAC output
    val prevVal = VecInit.tabulate(p.cRows) { r =>
        VecInit.tabulate(p.cCols) { c =>
            // each element is a RegNext of the matching out(r)(c).bits
            RegNext(TPU.io.out(r)(c).bits, 0.S(p.ow.W))
        }
    }
    // Remembers how many intermediate MAC outputs remain
    val readsRemaining = RegInit(VecInit.tabulate(p.sysw)(_ =>
                                 VecInit.fill(p.sysw)(0.U(log2Ceil(p.sysw).W))))


    // MAC output is ready as long as thee are still outputs to be read
    for (row <- 0 until p.cRows) {
        for(col <- 0 until p.cCols) {
            TPU.io.out(row)(col).ready := readsRemaining(row)(col) =/= 0.U
        }
    }

    // When outresult is valid and nonzero and it can still be read, read it out
    for (row <- 0 until p.cRows) {
        for(col <- 0 until p.cCols) {
            when (TPU.io.out(row)(col).valid && TPU.io.out(row)(col).bits =/= prevVal(row)(col)) {
                cReg(row)(col) := TPU.io.out(row)(col).bits
                readsRemaining(row)(col) := readsRemaining(row)(col) - 1.U
            }
        }
    }    
    // Defaults

    // IO Data and Valid to Systolic Array Input
    for (col <- 0 until p.aCols) {
        TPU.io.inTop(col).valid := false.B
        TPU.io.inTop(col).bits  := 0.S
    }

    for (row <- 0 until p.aRows) {
        TPU.io.inLeft(row).valid := false.B
        TPU.io.inLeft(row).bits  := 0.S
    }

    io.out.bits  := cReg
    io.out.valid := false.B
    io.in.ready := true.B

    val topReadySignals = TPU.io.inTop.map(_.ready)
    val leftReadySignals = TPU.io.inLeft.map(_.ready)

    io.in.ready := topReadySignals.reduce(_ && _) && leftReadySignals.reduce(_ && _)


    switch(writeCycle.value) {
    // Flashes input matrices to local registers in state -1
        is(0.U){
            aReg := io.in.bits.a
            bReg := io.in.bits.b
            writeCycle.inc()  
        }
        is(1.U) {
            // First Write
            TPU.io.inLeft(0).valid := true.B
            TPU.io.inTop(0).valid := true.B

            TPU.io.inLeft(0).bits := aReg(0)(0)
            TPU.io.inTop(0).bits := bReg(0)(0)

            writeCycle.inc()  
        }
        is(2.U) {
            // Last write
            TPU.io.inLeft(0).valid := true.B
            TPU.io.inTop(0).valid := true.B
            TPU.io.inLeft(0).bits := aReg(0)(1)
            TPU.io.inTop(0).bits := bReg(1)(0)

            // First Write
            TPU.io.inLeft(1).valid := true.B
            TPU.io.inTop(1).valid := true.B
            TPU.io.inLeft(1).bits := aReg(1)(0)
            TPU.io.inTop(1).bits := bReg(0)(1)


            writeCycle.inc()  
        }
        is(3.U) {
            // Last write
            TPU.io.inLeft(1).valid := true.B
            TPU.io.inTop(1).valid := true.B
            TPU.io.inLeft(1).bits := aReg(1)(1)
            TPU.io.inTop(1).bits := bReg(1)(1)

            writeCycle.inc()  
        }
        is(4.U) {
            writeCycle.inc()  

        }
        is(5.U) {
            // Last read
            writeCycle.inc()  
        }
    }
    io.out.bits := cReg
    io.out.valid := writeCycle.value === 5.U 

    TPU.io.clear := io.clear
}