package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

case class TPUParams (inputWidth: Int, outputWidth: Int) {
    // TODO: parametrize matrix size.
    val iw: Int = inputWidth // Input bitwidth
    val ow: Int = outputWidth
    val s: Int = 2 // Systolic Array Size

}

class TPU(val p:TPUParams) {
    val io = IO(new Bundle {
        
    })

    // TODO: generate array of MACs based on size of output matrix
    ???
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