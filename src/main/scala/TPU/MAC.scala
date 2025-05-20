
import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

// adapted from https://github.com/AustinY2430/FPGA-Based-TPU/blob/main/Design/SYSMAC.v

class MAC(/*TODO: Put stuff here*/) extends Module {
    val io = IO(new Bundle {
        // TODO: add proper io.
        val inLeft = Input()
        val inTop = Input()
        val outRight = Output()
        val outBottom = Output()
        val outResult = Output()
    })
    val multiply = RegInit(0.S(32.W))
    val currResult = RegInit(0.S(32.W))

    outRight := inLeft
    outBottom := inTop
    // TODO: may b cycle off since registered
    currResult := currResult + multiply
    outResult := currResult
}