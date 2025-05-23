import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

clase class TPUParams () {
    // TODO: parametrize matrix size.
    val w: Int = 8 // datawidth

}

class TPU(val p:TPUParams) {
    val io = IO(new Bundle {

    })

    // TODO: generate array of MACs based on size of output matrix
}