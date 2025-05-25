package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

case class TPUParams (inputWidth: Int, outputWidth: Int) {
    // TODO: parametrize matrix size.
    val iw: Int = inputWidth // Input bitwidth
    val ow: Int = outputWidth
    
    val aRows: Int = 2
    val aCols: Int = 2

    val bRows: Int = 2
    val bCols: Int = 2

}

// class TPU(val p:TPUParams) {
//     val io = IO(new Bundle {
        
//     })

//     // TODO: generate array of MACs based on size of output matrix
//     ???
// }

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

    //TODO Implement ready as control logic
    io.inTopNE.ready  := true.B
    io.inTopNW.ready  := true.B
    io.inLeftNW.ready := true.B
    io.inLeftSW.ready := true.B

    // State information
    // TODO: Parametrize number of cycles in counter.
    // val OpCycles = new Counter(s + 1)

    // TODO: add state for idle, load, calculating, and complete
    // TODO: calculate matrix results.

        
}


class TPU_fixed_vec_io(p: TPUParams) extends Module {
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
    val nwReadsLeft = RegInit(0.U(2.W))
    val neReadsLeft = RegInit(0.U(2.W))
    val swReadsLeft = RegInit(0.U(2.W))
    val seReadsLeft = RegInit(0.U(2.W))

    // When register is zero, MAC value is finalized
    TPU.io.outNW.ready := nwReadsLeft =/= 0.U
    TPU.io.outNE.ready := neReadsLeft =/= 0.U
    TPU.io.outSW.ready := swReadsLeft =/= 0.U
    TPU.io.outSE.ready := seReadsLeft =/= 0.U

    // When outresult is valid and nonzero and it can still be read, read it out
    when (TPU.io.outNW.fire) { cReg(0)(0) := TPU.io.outNW.bits ; nwReadsLeft := nwReadsLeft - 1.U }
    when (TPU.io.outNE.fire) { cReg(0)(1) := TPU.io.outNE.bits ; neReadsLeft := neReadsLeft - 1.U }
    when (TPU.io.outSW.fire) { cReg(1)(0) := TPU.io.outSW.bits ; swReadsLeft := swReadsLeft - 1.U }
    when (TPU.io.outSE.fire) { cReg(1)(1) := TPU.io.outSE.bits ; seReadsLeft := seReadsLeft - 1.U }
    
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
            nwReadsLeft := 2.U
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
            neReadsLeft := 2.U
            swReadsLeft := 2.U
            TPU.io.inTopNE.valid := true.B
            TPU.io.inLeftSW.valid := true.B
            TPU.io.inTopNE.bits := bReg(0)(1)
            TPU.io.inLeftSW.bits := aReg(1)(0)


            writeCycle.inc()  
        }
        is(3.U) {
            // Last write
            seReadsLeft := 2.U
            TPU.io.inTopNE.valid := true.B
            TPU.io.inLeftSW.valid := true.B
            TPU.io.inTopNE.bits := bReg(1)(1)
            TPU.io.inLeftSW.bits := aReg(1)(1)

            writeCycle.inc()  
        }
        is(4.U) {
            when (neReadsLeft === 0.U && swReadsLeft === 0.U) {
                writeCycle.inc()  
            }
        }
        is(5.U) {
            // Last read
            when (nwReadsLeft === 0.U && seReadsLeft === 0.U) {
                writeCycle.inc()  
            }
        }
    }
    io.out.bits := cReg
    io.out.valid := writeCycle.value === 4.U && nwReadsLeft === 0.U && neReadsLeft  === 0.U && swReadsLeft  === 0.U && seReadsLeft  === 0.U

    TPU.io.clear := io.clear
}