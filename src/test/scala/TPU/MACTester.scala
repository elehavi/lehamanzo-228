import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

// TODO: import model for MAC?

// TODO: rewrite for just MAC?
class MatMulSCTester extends AnyFlatSpec with ChiselScalatestTester {
  def doMacTest(/*TODO: define fcn*/): Unit = {
   
   // TODO: change this function
    test(new MAC(/*params here TODO*/)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // TODO test goes here?
    }
  }

  behavior of "MAC"
    // TODO: implement actual tests
  it should "multiply (1s row) x (1s column)" in {
    val k = 4
    doMatMulSCTest(MatMulTestData.genOnesRow(k), MatMulTestData.genOnesCol(k), 1)
  }


}