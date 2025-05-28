package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

case class TPUParams (inputWidth: Int, outputWidth: Int, systolicArrayWidth: Int) {
    // TODO: parametrize matrix size.
    val iw: Int = inputWidth // Input bitwidth
    val ow: Int = outputWidth
    
    val sysW: Int = systolicArrayWidth
    val maxWrites = (systolicArrayWidth * 2)
    val finalReads = (systolicArrayWidth - 1)

    val aRows: Int = systolicArrayWidth
    val aCols: Int = systolicArrayWidth

    val bRows: Int = systolicArrayWidth
    val bCols: Int = systolicArrayWidth
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
    val macNE = Module(new MAC(macParams(p.iw, p.ow))) // inLeft, inTop, outRight, outBottom, outResult
    val macNW = Module(new MAC(macParams(p.iw, p.ow)))
    val macSE = Module(new MAC(macParams(p.iw, p.ow)))
    val macSW = Module(new MAC(macParams(p.iw, p.ow)))


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
    val nwmacReadsRemaining = RegInit(0.U(2.W))
    val nemacReadsRemaining = RegInit(0.U(2.W))
    val swmacReadsRemaining = RegInit(0.U(2.W))
    val semacReadsRemaining = RegInit(0.U(2.W))

    // When register is zero, MAC value is finalized
    TPU.io.outNW.ready := nwmacReadsRemaining =/= 0.U
    TPU.io.outNE.ready := nemacReadsRemaining =/= 0.U
    TPU.io.outSW.ready := swmacReadsRemaining =/= 0.U
    TPU.io.outSE.ready := semacReadsRemaining =/= 0.U

    // When outresult is valid and nonzero and it can still be read, read it out
    when (TPU.io.outNW.fire) { cReg(0)(0) := TPU.io.outNW.bits ; nwmacReadsRemaining := nwmacReadsRemaining - 1.U }
    when (TPU.io.outNE.fire) { cReg(0)(1) := TPU.io.outNE.bits ; nemacReadsRemaining := nemacReadsRemaining - 1.U }
    when (TPU.io.outSW.fire) { cReg(1)(0) := TPU.io.outSW.bits ; swmacReadsRemaining := swmacReadsRemaining - 1.U }
    when (TPU.io.outSE.fire) { cReg(1)(1) := TPU.io.outSE.bits ; semacReadsRemaining := semacReadsRemaining - 1.U }
    
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
            nwmacReadsRemaining := 2.U
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
            nemacReadsRemaining := 2.U
            swmacReadsRemaining := 2.U
            TPU.io.inTopNE.valid := true.B
            TPU.io.inLeftSW.valid := true.B
            TPU.io.inTopNE.bits := bReg(0)(1)
            TPU.io.inLeftSW.bits := aReg(1)(0)


            writeCycle.inc()  
        }
        is(3.U) {
            // Last write
            semacReadsRemaining := 2.U
            TPU.io.inTopNE.valid := true.B
            TPU.io.inLeftSW.valid := true.B
            TPU.io.inTopNE.bits := bReg(1)(1)
            TPU.io.inLeftSW.bits := aReg(1)(1)

            writeCycle.inc()  
        }
        is(4.U) {
            when (nemacReadsRemaining === 0.U && swmacReadsRemaining === 0.U) {
                writeCycle.inc()  
            }
        }
        is(5.U) {
            // Last read
            when (nwmacReadsRemaining === 0.U && semacReadsRemaining === 0.U) {
                writeCycle.inc()  
            }
        }
    }
    io.out.bits := cReg
    io.out.valid := writeCycle.value === 4.U && nwmacReadsRemaining === 0.U && nemacReadsRemaining  === 0.U && swmacReadsRemaining  === 0.U && semacReadsRemaining  === 0.U

    TPU.io.clear := io.clear
}

class TPU_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        val clear  = Input(Bool())

        // Vector inputs corresponding to top and left systolic array inputs
        // Vector inputs corresponding to top and left systolic array inputs
        val inTop  = Flipped(Vec(p.aCols,  Decoupled(SInt(p.iw.W))))
        val inLeft = Flipped(Vec(p.aRows,  Decoupled(SInt(p.iw.W))))

        val out    = Vec(p.aRows, Vec(p.bCols, Decoupled(SInt(p.ow.W))))
    })

    // Systolic Array parameterized by TPU params
    // Systolic Array parameterized by TPU params
    val systolicArray: Seq[Seq[MAC]] =
        Seq.fill(p.sysW, p.sysW)(Module(new MAC(macParams(p.iw, p.ow))))

    // ----------------------------------------------- Systolic Array IO -----------------------------------------------
    // IO Data and Valid to Systolic Array Input

    // Link data and valid inputs from TPU io to Systolic Array
    for (i <- 0 until p.sysW) {
        systolicArray(0)(i).io.inTop.valid := io.inTop(i).valid
        systolicArray(0)(i).io.inTop.bits  := io.inTop(i).bits
        io.inTop(i).ready                  := true.B

        systolicArray(i)(0).io.inLeft.valid := io.inLeft(i).valid
        systolicArray(i)(0).io.inLeft.bits  := io.inLeft(i).bits
        io.inLeft(i).ready                  := true.B
    }


    // Link systolic array outputs to TPU io
    for (row <- 0 until p.sysW) {
        for(col <- 0 until p.sysW) {
            // IO Bits and Valid
            io.out(row)(col).bits  := systolicArray(row)(col).io.outResult.bits
            io.out(row)(col).valid := systolicArray(row)(col).io.outResult.valid
            // IO Ready 
            
            systolicArray(row)(col).io.outResult.ready := io.out(row)(col).ready
            // Clear
            systolicArray(row)(col).io.clear           := io.clear
        }
    }

    // ----------------------------------------------- Systolic Array Internal Wiring -----------------------------------------------
    // Ready horizontal connections between MACs
    for(row <- 0 until p.sysW) {
        for(col <- 0 until p.sysW) {
            // first element ignore valid connection (goes to IO) 
            if (col > 0) {
                    systolicArray(row)(col - 1).io.outRight.ready := systolicArray(row)(col).io.inLeft.ready
                    systolicArray(row)(col).io.inLeft.valid       := systolicArray(row)(col - 1).io.outRight.valid
                    systolicArray(row)(col).io.inLeft.bits        := systolicArray(row)(col - 1).io.outRight.bits
            }
            // Last element ignore ready, (drains to edge)
            if (col == p.sysW - 1) {
                systolicArray(row)(col).io.outRight.ready := true.B
            }
        }
    }
    // Ready vertical connections between MACs
    for(col <- 0 until p.sysW) {
        for(row <- 0 until p.sysW) {
            // first element ignore valid connection (goes to IO) 
            if (row > 0) {
                systolicArray(row - 1)(col).io.outBottom.ready := systolicArray(row)(col).io.inTop.ready
                systolicArray(row)(col).io.inTop.valid         := systolicArray(row - 1)(col).io.outBottom.valid
                systolicArray(row)(col).io.inTop.bits          := systolicArray(row - 1)(col).io.outBottom.bits
            }
            // Last element ignore ready, (drains to edge)
            if (row == p.sysW - 1) {
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

    val nwmacReadsRemaining = RegInit(0.U(2.W))
    val nemacReadsRemaining = RegInit(0.U(2.W))
    val swmacReadsRemaining = RegInit(0.U(2.W))
    val semacReadsRemaining = RegInit(0.U(2.W))

    // When outresult is valid and nonzero and it can still be read, read it out
    when (NWChanged && TPU.io.out(0)(0).valid) { cReg(0)(0) := TPU.io.out(0)(0).bits ; nwmacReadsRemaining := nwmacReadsRemaining - 1.U }
    when (NEChanged && TPU.io.out(0)(1).valid) { cReg(0)(1) := TPU.io.out(0)(1).bits ; nemacReadsRemaining := nemacReadsRemaining - 1.U }
    when (SWChanged && TPU.io.out(1)(0).valid) { cReg(1)(0) := TPU.io.out(1)(0).bits ; swmacReadsRemaining := swmacReadsRemaining - 1.U }
    when (SEChanged && TPU.io.out(1)(1).valid) { cReg(1)(1) := TPU.io.out(1)(1).bits ; semacReadsRemaining := semacReadsRemaining - 1.U }


    // When register is zero, MAC value is finalized
    TPU.io.out(0)(0).ready := nwmacReadsRemaining =/= 0.U
    TPU.io.out(0)(1).ready := nemacReadsRemaining =/= 0.U
    TPU.io.out(1)(0).ready := swmacReadsRemaining =/= 0.U
    TPU.io.out(1)(1).ready := semacReadsRemaining =/= 0.U
    
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
    // --------------------------------------------Systolic Array and Mem--------------------------------------------
    val TPU  = Module(new TPU_parameterized(p))

    // Local memory to flash input matrices to in idle state
    // Local memory to flash input matrices to in idle state
    val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W))))
    val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W))))
    // Initialize output mat to zeros
    val cReg = RegInit(VecInit.tabulate(p.aRows)(_ =>
                       VecInit.fill(p.bCols)(0.S(p.ow.W))))

    // -----------------------------------------------Output Read Logic-----------------------------------------------

    // Stores previous values of each MAC output
    // Detects if change occurs between clock cycles
    val prevMacVal = VecInit.tabulate(p.sysW) { r =>
        VecInit.tabulate(p.sysW) { c =>
            // each element is a RegNext of the matching out(r)(c).bits
            RegNext(TPU.io.out(r)(c).bits, 0.S(p.ow.W))
        }
    }
    // Remembers how many intermediate MAC outputs remain
    val macReadsRemaining = RegInit(VecInit.tabulate(p.sysW)(_ =>
                                 VecInit.fill(p.sysW)(0.U(log2Ceil(p.sysW).W))))

    // MAC output is ready as long as there are still outputs to be read
    for (row <- 0 until p.sysW) {
        for(col <- 0 until p.sysW) {
            TPU.io.out(row)(col).ready := macReadsRemaining(row)(col) =/= 0.U
        }
    }
    // When outresult is valid and changed, read out and decrement read counter
    for (row <- 0 until p.sysW) {
        for(col <- 0 until p.sysW) {
            val macOutputValid = TPU.io.out(row)(col).valid
            val macOutputChanged = TPU.io.out(row)(col).bits =/= prevMacVal(row)(col)
            when (macOutputValid && macOutputChanged) {
                cReg(row)(col)           := TPU.io.out(row)(col).bits
                // Decrement reads left for MAC output
                macReadsRemaining(row)(col) := macReadsRemaining(row)(col) - 1.U
            }
        }
    }    
    // -----------------------------------------------FSM Logic and Mem-----------------------------------------------
    object TPUState extends ChiselEnum { val idle, writing, reading, complete = Value }
    val state      = RegInit(TPUState.idle)

    val writeCycle = new Counter(p.maxWrites)
    val readCycle  = new Counter(p.finalReads)

    // ----------------------------------------------FSM State Definition----------------------------------------------
    for (i <- 0 until p.sysW) {
        // Restricts each input to send within the appropriate window
        // Staggered each cycle from NW MAC, and should total the width of the systolic array
        val sendWindow = (writeCycle.value >= i.U) && (writeCycle.value <  (i + p.sysW).U)
        // When in send state and valid window, assign matrix element to corresponding
        // systolic data input based on clock cycle
        TPU.io.inLeft(i).valid := (state === TPUState.writing) && sendWindow
        TPU.io.inLeft(i).bits  := aReg(i)(writeCycle.value - i.U)

        TPU.io.inTop(i).valid := (state === TPUState.writing) && sendWindow
        TPU.io.inTop(i).bits  := bReg(writeCycle.value - i.U)(i)
    }

    // ---------------------------------------------------Defaults---------------------------------------------------

    io.out.bits  := cReg
    io.out.valid := false.B
    io.in.ready  := true.B
    TPU.io.clear := io.clear

    // -----------------------------------------------------FSM-----------------------------------------------------

    switch(state) {
        // When idle, flash local Matrix A and B storage
        is(TPUState.idle) {
            aReg  := io.in.bits.a
            bReg  := io.in.bits.b
            state := TPUState.writing  
        }
        // When writing, data is written to each inTop and inLeft referring to writeCycle counter value
        is(TPUState.writing) {
            when (writeCycle.value === p.maxWrites.U - 1.U) { state := TPUState.reading }
            .otherwise                                      { writeCycle.inc() }
        }
        // After final write, remaining reads take place
        is(TPUState.reading) {
            when (readCycle.value === p.finalReads.U - 1.U) { state := TPUState.complete }
            .otherwise                                      { readCycle.inc() }
        }
        // TPU holds computed Matrix C on output until cleared
        is(TPUState.complete) {
            writeCycle.value := 0.U
            readCycle.value  := 0.U
            // Systolic array valid when final (SE) MAC is valid
            io.out.bits  := cReg
            io.out.valid := TPU.io.out(p.sysW.U - 1.U)(p.sysW.U - 1.U).valid
            
            when (io.clear) { state := TPUState.idle }
        }
    }
}