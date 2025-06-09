package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

case class TPUParams (inputWidth: Int, outputWidth: Int, systolicArrayWidth: Int, m: Int, k: Int, n: Int) {
    // A (m x k) X B (k x n) = C (m x n)
    val iw: Int = inputWidth
    val ow: Int = outputWidth

    val sysW: Int = systolicArrayWidth

    val aRows: Int = m
    val aCols: Int = k

    val bRows: Int = k
    val bCols: Int = n

    val cRows: Int = m
    val cCols: Int = n
    
    val maxWrites = (aCols + systolicArrayWidth)
    val finalReads = (systolicArrayWidth + 1)

    // Make sure matrices are compatible for multiplication
    assert(aCols == bRows)
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
    // Systolic Array parameterized by TPU params
    val systolicArray: Seq[Seq[MAC]] =
        Seq.fill(p.sysW, p.sysW)(Module(new MAC(macParams(p.iw, p.ow))))

    // ----------------------------------------------- Systolic Array IO -----------------------------------------------
    // IO Data and Valid to Systolic Array Input

    // Link data and valid inputs from TPU io to Systolic Array
    for (i <- 0 until p.sysW) {
        if (i < p.bCols) {
            systolicArray(0)(i).io.inTop <> io.inTop(i)
        } else {
            systolicArray(0)(i).io.inTop.valid := false.B
            systolicArray(0)(i).io.inTop.bits := 0.S
        }
        if ( i < p.aRows) {
            systolicArray(i)(0).io.inLeft <> io.inLeft(i)
        } else {
            systolicArray(i)(0).io.inLeft.valid := false.B
            systolicArray(i)(0).io.inLeft.bits := 0.S
        }
        io.inTop(i).ready  := true.B
        io.inLeft(i).ready := true.B
    }


    // Connect outResult outputs from systolic array
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
            if (col == p.sysW - 1) {
                // Defaults since drains to edge
                systolicArray(row)(col).io.outRight.ready := true.B
            }
        }
    }
    // Ready vertical connections between MACs
    for(col <- 0 until p.sysW) {
        for(row <- 0 until p.sysW) {
            if (row > 0) {
                systolicArray(row - 1)(col).io.outBottom.ready := systolicArray(row)(col).io.inTop.ready
                systolicArray(row)(col).io.inTop.valid         := systolicArray(row - 1)(col).io.outBottom.valid
                systolicArray(row)(col).io.inTop.bits          := systolicArray(row - 1)(col).io.outBottom.bits
            }
            if (row == p.sysW - 1) {
                // Defaults since drains to edge
                systolicArray(row)(col).io.outBottom.ready := true.B
            }
        }
    }
}

class Top_parameterized(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        
        val clear = Input(Bool())
        // Vector input and output matrices
        val in = Flipped(Decoupled(new Bundle {
            val a = Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W)))
            val b = Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W)))
        }))
        val out = Valid(Vec(p.sysW, Vec(p.sysW, SInt(p.ow.W))))

    })
    // --------------------------------------------Systolic Array and Mem--------------------------------------------
    val TPU  = Module(new TPU_parameterized(p))

    // Local memory to flash input matrices to in idle state
    // Local memory to flash input matrices to in idle state
    val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.iw.W))))
    val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.iw.W))))

    // Initialize output mat to zeros
    val cReg = RegInit(VecInit.tabulate(p.sysW)(_ =>
                       VecInit.fill(p.sysW)(0.S(p.ow.W))))

    // Representaion Matrices and wires for naive sparse implementation
    val aRep = Reg(Vec(p.aRows, Vec(p.aCols, Bool())))
    val bRep = Reg(Vec(p.bRows, Vec(p.bCols, Bool())))

    val aRepWire = Wire(Vec(p.aRows, Vec(p.aCols, Bool())))
    val bRepWire = Wire(Vec(p.bRows, Vec(p.bCols, Bool())))

    // Initialize Representation Matrices (true if data, false if zero)
    for (row <- 0 until p.aRows; col <- 0 until p.aCols) {
        aRepWire(row)(col) := aReg(row)(col) =/= 0.S
    }
    for (row <- 0 until p.bRows; col <- 0 until p.bCols) {
        bRepWire(row)(col) := bReg(row)(col) =/= 0.S
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

    val partialProductsPerTile = p.aCols
    for (i <- 0 until p.sysW) {
        // Index for partial product addressing
        val ppIdx    = writeCycle.value - i.U
        // Restricts each input to send within the appropriate window
        // Staggered each cycle from NW MAC, and should total the width of the systolic array
        val sendWindow = ppIdx < partialProductsPerTile.U

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

        TPU.io.inLeft(i).bits  := Mux(rowA_Data && colA_Data && aRepWire(aRow)(aCol),
                                    aReg(aRow)(aCol),
                                    0.S)

        TPU.io.inTop(i).valid  := (state === TPUState.writing) && sendWindow && rowB_Data && colB_Data

        TPU.io.inTop(i).bits   := Mux(rowB_Data && colB_Data && bRepWire(bRow)(bCol),
                                    bReg(bRow)(bCol),
                                    0.S)
    }
    // -----------------------------------------------Output Read Logic-----------------------------------------------
    
    // cycles depends on common dimension aCols/bRows 
    val macReadsRemaining = RegInit(VecInit.tabulate(p.sysW)(_ =>
                                 VecInit.fill(p.sysW)(partialProductsPerTile.U)))

    // MAC output is ready as long as there are still outputs to be read
    for (row <- 0 until p.sysW) {
        for(col <- 0 until p.sysW) {
            TPU.io.out(row)(col).ready := macReadsRemaining(row)(col) =/= 0.U
            // TPU out is valid and ready
            when (TPU.io.out(row)(col).fire) {
                cReg(row)(col) := TPU.io.out(row)(col).bits
                macReadsRemaining(row)(col) := macReadsRemaining(row)(col) - 1.U
            }
            when (state === TPUState.rewind) {
                macReadsRemaining(row)(col) := partialProductsPerTile.U
                cReg(row)(col)              := 0.S
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