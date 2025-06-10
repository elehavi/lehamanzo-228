package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._
import os.write

case class TPUParams (inputWidth: Int, outputWidth: Int, systolicArrayWidth: Int, m: Int, k: Int, n: Int, conv: Boolean) {
    // A (m x k) X B (k x n) = C (m x n)

    // If Conv, m = kernel height and width (square)
    //          k = channels (one for now)
    //          n = input activation height and width (square)
    val iw: Int = inputWidth
    val ow: Int = outputWidth

    val sysW: Int = systolicArrayWidth

    // Common dimension
    val partialProductsPerTile: Int = if(conv) m*m*k else k

    // If convolution, kernel is mxm
    val aRows: Int = m
    val aCols: Int = if(conv) m else k

    // If convolution, input activation is nxn
    val bRows: Int = if(conv) n else k
    val bCols: Int = n
    
    // If convolution, output is a single line with dimensions of output activation
    val cRows: Int = if(conv) 1 else aRows
    val cCols: Int = if(conv) (n-1)*(n-1) else bCols

    // FSM timing definitions
    val maxWrites = (partialProductsPerTile + systolicArrayWidth)
    val finalReads = (systolicArrayWidth + 1)
}

class TPU_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        val clear  = Input(Bool())

        // Vector inputs corresponding to top and left systolic array inputs
        // Fixed to specified systolic array width
        val inTop  = Flipped(Vec(p.sysW,  Decoupled(SInt(p.iw.W))))
        val inLeft = Flipped(Vec(p.sysW,  Decoupled(SInt(p.iw.W))))

        val out    = Vec(p.sysW, Vec(p.sysW, Decoupled(SInt(p.ow.W))))
    })

    // Systolic Array parameterized by TPU params
    val systolicArray: Seq[Seq[MAC]] =
        Seq.fill(p.sysW, p.sysW)(Module(new MAC(macParams(p.iw, p.ow))))

    // ----------------------------------------------- Systolic Array IO -----------------------------------------------
    // IO Data and Valid to Systolic Array Input

    // Link data and valid inputs from TPU io to Systolic Array
    for (i <- 0 until p.sysW) {
        systolicArray(0)(i).io.inTop  <> io.inTop(i)
        systolicArray(i)(0).io.inLeft <> io.inLeft(i)
        
        io.inTop(i).ready  := true.B
        io.inLeft(i).ready := true.B
    }

    // Connect outResult outputs from systolic array
    for (r <- 0 until p.sysW) {
        for(c <- 0 until p.sysW) {
            io.out(r)(c).bits  := systolicArray(r)(c).io.outResult.bits
            io.out(r)(c).valid := systolicArray(r)(c).io.outResult.valid

            systolicArray(r)(c).io.outResult.ready := io.out(r)(c).ready

            systolicArray(r)(c).io.clear           := io.clear
        }
    }

    // ----------------------------------------------- Systolic Array Internal Wiring -----------------------------------------------
    // Horizontal connections between MACs
    for(r <- 0 until p.sysW) {
        for(c <- 0 until p.sysW) {
            if (c > 0) {
                    systolicArray(r)(c - 1).io.outRight.ready := systolicArray(r)(c).io.inLeft.ready
                    systolicArray(r)(c).io.inLeft.valid       := systolicArray(r)(c - 1).io.outRight.valid
                    systolicArray(r)(c).io.inLeft.bits        := systolicArray(r)(c - 1).io.outRight.bits
            }
            // Defaults since drains to edge
            if (c == p.sysW - 1) {
                systolicArray(r)(c).io.outRight.ready := true.B
            }
        }
    }
    // Vertical connections between MACs
    for(c <- 0 until p.sysW) {
        for(r <- 0 until p.sysW) {
            if (r > 0) {
                systolicArray(r - 1)(c).io.outBottom.ready := systolicArray(r)(c).io.inTop.ready
                systolicArray(r)(c).io.inTop.valid         := systolicArray(r - 1)(c).io.outBottom.valid
                systolicArray(r)(c).io.inTop.bits          := systolicArray(r - 1)(c).io.outBottom.bits
            }
            // Defaults since drains to edge
            if (r == p.sysW - 1) {
                systolicArray(r)(c).io.outBottom.ready := true.B
            }
        }
    }
}

class Top_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        val clear = Input(Bool())

        val in = Flipped(Decoupled(new Bundle {
            val a = Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W)))
            val b = Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W)))
        }))
        val out = Valid(Vec(p.sysW, Vec(p.sysW, SInt(p.ow.W))))
    })
    // --------------------------------------------Systolic Array and Mem--------------------------------------------
    val TPU  = Module(new TPU_parameterized(p))

    // Local memory to flash input matrices to in idle state
    val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W))))
    val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W))))

    // Initialize output mat to zeros
    val cReg = RegInit(VecInit.tabulate(p.sysW)(_ =>
                       VecInit.fill(p.sysW)(0.S(p.ow.W))))

    // Representaion Matrices and wires for sparse awareness
    val aRep = Reg(Vec(p.aRows, Vec(p.aCols, Bool())))
    val bRep = Reg(Vec(p.bRows, Vec(p.bCols, Bool())))

    val aRepWire = Wire(Vec(p.aRows, Vec(p.aCols, Bool())))
    val bRepWire = Wire(Vec(p.bRows, Vec(p.bCols, Bool())))

    // Initialize Representation Matrices (true if data, false if zero)
    for (r <- 0 until p.aRows; c <- 0 until p.aCols) {
        aRepWire(r)(c) := aReg(r)(c) =/= 0.S
    }
    for (r <- 0 until p.bRows; c <- 0 until p.bCols) {
        bRepWire(r)(c) := bReg(r)(c) =/= 0.S
    }
    // -----------------------------------------------FSM Logic and Mem-----------------------------------------------
    object TPUState extends ChiselEnum { val idle, writing, reading, rewind, complete = Value }
    val state      = RegInit(TPUState.idle)

    val writeCycle = new Counter(p.maxWrites)
    val readCycle  = new Counter(p.finalReads)

    // Ceiling division for tile number
    val rowTiles  = (p.aRows + p.sysW - 1) / p.sysW
    val colTiles  = (p.bCols + p.sysW - 1) / p.sysW
    val tileNum   = rowTiles*colTiles

    // Addressing tiles in column major order
    val nTiles = math.max(1,rowTiles*colTiles)
    val tileIdx = RegInit(0.U(log2Ceil(nTiles).W))

    // # columns is how many rows processed so far
    val colIdx = tileIdx / rowTiles.U
    // # of rows per column is the remainder
    val rowIdx = tileIdx % rowTiles.U
    // ----------------------------------------------FSM State Definition----------------------------------------------
    if (p.conv) {
        for (i <- 0 until p.sysW) {
            val ppIdx    = writeCycle.value - i.U
            // Index for partial product addressing
            // Restricts each input to send within the appropriate window
            // Staggered each cycle from NW MAC, and should total the width of the systolic array
            val sendWindow = ppIdx < p.partialProductsPerTile.U

            // im2Col implementation
            val startRow = i.U / (p.n - p.m + 1).U
            val startcol = i.U % (p.n - p.m + 1).U

            val rowOffset = ppIdx / p.m.U
            val colOffset = ppIdx % p.m.U

            // A indices only offset from zero
            val aRow = rowOffset
            val aCol = colOffset

            // B indices offset from start position in input activation
            val bRow = startRow + rowOffset
            val bCol = startcol + colOffset

            val rowA_Data  = aRow < p.aRows.U
            val colA_Data  = aCol < p.aCols.U
            
            val rowB_Data  = bRow < p.bRows.U
            val colB_Data  = bCol < p.bCols.U

            // When in send state and valid window, assign matrix element to corresponding
            // systolic data input based on clock cycle

            TPU.io.inLeft(i).valid := (state === TPUState.writing) && sendWindow && rowA_Data && colA_Data
            TPU.io.inTop(i).valid  := (state === TPUState.writing) && sendWindow && rowB_Data && colB_Data

            TPU.io.inLeft(i).bits  := Mux(rowA_Data && colA_Data && aRepWire(aRow)(aCol), aReg(aRow)(aCol), 0.S)
            TPU.io.inTop(i).bits   := Mux(rowB_Data && colB_Data && bRepWire(bRow)(bCol), bReg(bRow)(bCol), 0.S)

        }
    }
    else {
        for (i <- 0 until p.sysW) {
            val ppIdx    = writeCycle.value - i.U
            // Index for partial product addressing
            // Restricts each input to send within the appropriate window
            // Staggered each cycle from NW MAC, and should total the width of the systolic array
            val sendWindow = ppIdx < p.partialProductsPerTile.U

            // Row to assign to inleft vector
            val aRow = rowIdx * p.sysW.U + i.U

            // Common Dimension
            val aCol = ppIdx
            val bRow = ppIdx

            // Col to assign to inTop vector
            val bCol = colIdx * p.sysW.U + i.U

            val rowA_Data  = aRow < p.aRows.U
            val colA_Data  = aCol < p.aCols.U
            
            val rowB_Data  = bRow < p.bRows.U
            val colB_Data  = bCol < p.bCols.U

            // When in send state and valid window, assign matrix element to corresponding
            // systolic data input based on clock cycle
            TPU.io.inLeft(i).valid := (state === TPUState.writing) && sendWindow && rowA_Data && colA_Data
            TPU.io.inTop(i).valid  := (state === TPUState.writing) && sendWindow && rowB_Data && colB_Data

            TPU.io.inLeft(i).bits  := Mux(rowA_Data && colA_Data && aRepWire(aRow)(aCol), aReg(aRow)(aCol), 0.S)
            TPU.io.inTop(i).bits   := Mux(rowB_Data && colB_Data && bRepWire(bRow)(bCol), bReg(bRow)(bCol), 0.S)
        }
    }
    // -----------------------------------------------Output Read Logic-----------------------------------------------
    // cycles depends on common dimension aCols/bRows 
    val macReadsRemaining = RegInit(VecInit.tabulate(p.sysW)(_ =>
                                 VecInit.fill(p.sysW)(p.partialProductsPerTile.U)))

    // MAC output is ready as long as there are still outputs to be read
    for (r <- 0 until p.sysW) {
        for(c <- 0 until p.sysW) {
            TPU.io.out(r)(c).ready := macReadsRemaining(r)(c) =/= 0.U
            // TPU out is valid and ready
            when (TPU.io.out(r)(c).fire) {
                cReg(r)(c) := TPU.io.out(r)(c).bits
                macReadsRemaining(r)(c) := macReadsRemaining(r)(c) - 1.U
            }
            when (state === TPUState.rewind) {
                macReadsRemaining(r)(c) := p.partialProductsPerTile.U
                cReg(r)(c)              := 0.S
            }
        }
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
            TPU.io.clear := true.B  
            state := TPUState.writing
        }
        // When writing, data is written to each inTop and inLeft referring to writeCycle counter value
        is(TPUState.writing) {
            TPU.io.clear := false.B
            when (writeCycle.value === p.maxWrites.U - 1.U) { state := TPUState.reading }
            .otherwise                                      { writeCycle.inc() }
        }
        // After final write, remaining reads take place
        is(TPUState.reading) {
            when (readCycle.value === (p.finalReads - 1).U) { 
                when (tileIdx === (tileNum - 1).U) { state := TPUState.complete }
                .otherwise { state := TPUState.rewind }
            }
            .otherwise { readCycle.inc() }
        }
        // Extra cycle to reset counters and systolic array state
        is(TPUState.rewind) {
            readCycle.reset()
            writeCycle.reset()
            tileIdx := tileIdx + 1.U
            TPU.io.clear := true.B
            state := TPUState.writing
        }
        // TPU holds computed Matrix C on output until cleared
        is(TPUState.complete) {
            readCycle.reset()
            writeCycle.reset()
            // Systolic array valid when final (SE) MAC is valid
            io.out.bits  := cReg
            io.out.valid := TPU.io.out(p.sysW.U - 1.U)(p.sysW.U - 1.U).valid
            
            when (io.clear) { state := TPUState.idle }
        }
    }
}