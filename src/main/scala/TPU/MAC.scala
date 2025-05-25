package tpu 

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._


case class MACParams(inputWidth: Int, outputWidth: Int) {
    val iw: Int = inputWidth // input width
    val ow: Int = outputWidth // output width
}


class MAC(p: MACParams) extends Module {
    val io = IO(new Bundle {
        val clear = Input(Bool())
        val inLeft = Flipped(Decoupled(SInt(p.iw.W)))
        val inTop = Flipped(Decoupled(SInt(p.iw.W)))

        val outRight = Decoupled(SInt(p.iw.W))
        val outBottom = Decoupled(SInt(p.iw.W))
        val outResult = Decoupled(SInt((p.ow).W))
    })

    val acc = RegInit(0.S(p.ow.W))

    val rightData = Reg(SInt(p.iw.W))
    val rightValid = RegInit(false.B)
    val downData = Reg(SInt(p.iw.W))
    val downValid = RegInit(false.B)

    // Latch propagating values until next cycle
    io.outRight.bits := rightData
    io.outRight.valid := rightValid

    io.outBottom.bits := downData
    io.outBottom.valid := downValid

    // latch outResult and only output when both inputs are valid data
    io.outResult.bits := acc
    io.outResult.valid := RegNext(io.inLeft.fire && io.inTop.fire, false.B)
    
    // Ready for more data if successfully passed to neighbor
    io.inLeft.ready := !rightValid || io.outRight.ready
    io.inTop.ready := !downValid || io.outBottom.ready

    // Once data is accumulated once, its no longer valid
    when (io.outRight.ready)  { rightValid := false.B }
    when (io.outBottom.ready) { downValid  := false.B }

    // When clear, reset accumulated result
    when(io.clear) {
        acc := 0.S
        rightValid := false.B
        downValid := false.B
    } .elsewhen (io.inLeft.fire && io.inTop.fire) {
        // Multiply and Accumulate
        acc := acc + (io.inLeft.bits * io.inTop.bits)

        // Set Registers and propagate values
        rightData := io.inLeft.bits
        rightValid := true.B

        downData := io.inTop.bits
        downValid := true.B
    } 
}
