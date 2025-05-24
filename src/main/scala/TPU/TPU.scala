package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

case class TPUParams (width: Int) {
    // TODO: parametrize matrix size.
    val w: Int = width // datawidth

}

class TPU(val p:TPUParams) {
    val io = IO(new Bundle {
        ???
    })

    // TODO: generate array of MACs based on size of output matrix
    ???
}

class TPU_fixed(p: TPUParams) extends Module {
    val io = IO(new Bundle {
        val inTopNE = Flipped(Decoupled(SInt(p.w.W)))
        val inTopNW = Flipped(Decoupled(SInt(p.w.W)))
        val inLeftNW = Flipped(Decoupled(SInt(p.w.W)))
        val inLeftSW = Flipped(Decoupled(SInt(p.w.W)))

        val outRightNE = Decoupled(SInt(p.w.W))
        val outRightSE = Decoupled(SInt(p.w.W))
        val outBottomSW = Decoupled(SInt(p.w.W))
        val outBottomSE = Decoupled(SInt(p.w.W))

        val outResult = Decoupled(Vec(2, Vec(2,SInt(p.w.W)))) 
    })
    // TODO: Parameterize generation of MACS, hardcode for now
    val macNE = Module(new MAC(MACParams(p.w))) // inLeft, inTop, outRight, outBottom, outResult
    val macNW = Module(new MAC(MACParams(p.w)))
    val macSE = Module(new MAC(MACParams(p.w)))
    val macSW = Module(new MAC(MACParams(p.w)))



    // Ready-Valid connections between MACs
    macNE.io.inLeft.valid := macNW.io.outRight.valid
    macSW.io.inTop.valid := macNW.io.outBottom.valid
    macSE.io.inTop.valid := macNE.io.outBottom.valid
    macSE.io.inLeft.valid := macSW.io.outRight.valid
    
    macNW.io.outRight.ready := macNE.io.inLeft.ready
    macNW.io.outBottom.ready := macSW.io.inTop.ready
    macSW.io.outRight.ready := macSE.io.inLeft.ready
    macNE.io.outBottom.ready := macSE.io.inTop.ready
    
    // Ready-Valid connections for IO
    io.outRightNE.valid := macNE.io.outRight.valid
    io.outRightSE.valid := macSE.io.outRight.valid
    io.outBottomSE.valid := macSE.io.outBottom.valid
    io.outBottomSW.valid := macSW.io.outBottom.valid

    io.inLeftNW.ready := macNW.io.inLeft.ready
    io.inLeftSW.ready := macSW.io.inLeft.ready
    io.inTopNE.ready := macNE.io.inTop.ready
    io.inTopNW.ready := macNW.io.inTop.ready
    
    // Inputs to Systolic Array
    macNE.io.inTop.bits := io.inTopNE.bits
    macNW.io.inTop.bits := io.inTopNW.bits
    macNW.io.inLeft.bits := io.inLeftNW.bits
    macSW.io.inLeft.bits := io.inLeftSW.bits

    // Connections between MACS
    macSW.io.inTop.bits := macNW.io.outBottom.bits
    macSE.io.inTop.bits := macNE.io.outBottom.bits
    macNW.io.inLeft.bits := macNE.io.outRight.bits
    macSW.io.inLeft.bits := macSE.io.outRight.bits

    // Outputs from Systolic Array
    io.outRightNE.bits := macNE.io.outRight.bits
    io.outRightSE.bits := macSE.io.outRight.bits
    io.outBottomSE.bits := macSE.io.outBottom.bits
    io.outBottomSW.bits := macSW.io.outRight.bits

    // State information
    // TODO: Parametrize number of cycles in counter.
    val OpCycles = new Counter(3)

    // TODO: add state for idle, load, calculating, and complete
    // TODO: calculate matrix results.

        
}