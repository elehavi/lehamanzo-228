
import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

// adapted from https://github.com/AustinY2430/FPGA-Based-TPU/blob/main/Design/SYSMAC.v

case class MACParams() {
    val w: Int = 8 // datawidth
}

// TODO: do we need reset?
class MAC(val p: MACParams) extends Module {
    val io = IO(new Bundle {
        // TODO: could this be bundled?
        val clear = Input(Bool())
        val inLeft = Flipped(Decoupled(SInt(p.w.W)))
        val inTop = Flipped(Decoupled(SInt(p.w.W)))

        val outRight = Decoupled(SInt(p.w.W))
        val outBottom = Decoupled(SInt(p.w.W))
        // 32 bits for now
        val outResult = Decoupled(SInt((4*p.w).W))
    })

    val currResult = RegInit(0.S(32.W))

    // Set Defaults

    // Output at t0 invalid
    io.outResult.valid := false.B
    io.outRight.valid := false.B
    io.outBottom.valid := false.B

    io.outResult.bits := 0.S
    io.outRight.bits := 0.S
    io.outBottom.bits := 0.S

    // Ready to receive data at t0
    io.inLeft.ready := true.B
    io.inTop.ready := true.B

    // When inputs are valid, accumulate and propagate
    when(io.inLeft.valid && io.inTop.valid) {
        // Multiply and Accumulate
        currResult := currResult + (io.inLeft.bits * io.inTop.bits)
        // Update register
        io.outResult.bits := currResult
        io.outRight.bits := io.inLeft.bits
        io.outBottom.bits := io.inTop.bits
        
        io.outResult.valid := true.B
        io.outRight.valid := true.B
        io.outBottom.valid := true.B
    
    // Otherwise if invalid, output zero and dont propagate
    } .elsewhen(io.clear) {
        // Dont read data this cycle
        io.inLeft.ready := false.B
        io.inTop.ready := false.B

        currResult := 0.S
        io.outResult.bits := 0.S
        io.outResult.valid := true.B
    }
}
