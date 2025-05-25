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
        // To reset internal Accumulators
        val clear = Input(Bool())
        // To receive and data input and output signal when ready to consume
        val inLeft = Flipped(Decoupled(SInt(p.iw.W)))
        val inTop = Flipped(Decoupled(SInt(p.iw.W)))

        // To produce output data and input signal when ready to produce
        val outRight = Decoupled(SInt(p.iw.W))
        val outBottom = Decoupled(SInt(p.iw.W))
        
        // To read out Accumulated result from MAC 
        val outResult = Decoupled(SInt((p.ow).W))
    })

    // Registers allow inputs to be propagated at next
    // clock cycle rather than short to output
    val rightData = Reg(SInt(p.iw.W))
    val downData = Reg(SInt(p.iw.W))
    val rightValid = RegInit(false.B)
    val downValid = RegInit(false.B)

    // output data wires connected to these registers
    io.outRight.bits := rightData
    io.outBottom.bits := downData
    io.outRight.valid := rightValid
    io.outBottom.valid := downValid

    // Invalid means we either have no data in reg, or have already accumululated
    // Valid means we haven't accumulated yet and will do so this clock cycle

    // Once data is accepted by it's neighbor (succesful fire)
    // Deassert valid so that data isn't duplicated on outputs
    when (io.outRight.fire)  { rightValid := false.B }
    when (io.outBottom.fire) { downValid  := false.B }

    // If invalid data that means we have an empty register or have already processed the previous
    // cycle's input, so we can accept the next input
    // OR if the neighbor is ready to consume, then we are ready to produce

    // Ready for more data if successfully passed to neighbor (invalid)
    // Or even if current MAC is processing, but neighbor is free to accept
    io.inLeft.ready := !rightValid || io.outRight.ready
    io.inTop.ready := !downValid || io.outBottom.ready

    // Accumulated Value
    val acc = RegInit(0.S(p.ow.W))

    // latch outResult and only output when both inputs are valid data
    io.outResult.bits := acc
    // When both inputs are ready and valid outResult is valid to be read on the next cycle
    // It will be false otherwise
    io.outResult.valid := RegNext(io.inLeft.fire && io.inTop.fire, false.B)
    

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
        downData := io.inTop.bits
        
        rightValid := true.B
        downValid := true.B
    } 
}
