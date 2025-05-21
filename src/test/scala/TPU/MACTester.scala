import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec


class MACTester extends AnyFlatSpec with ChiselScalatestTester {
  // def doMacTest(/*TODO: define fcn*/): Unit = {
   
  //  // TODO: change this function
  //  val p = new MACParams
  //   test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
  //     // TODO test goes here?
  //   }
  // }
    // TODO: implement actual tests
  /*  
    - properly passes inleft to outright
    - properly passes intop to outbottom
    - does NOT pass any data when inputs aren't valid
    
  */

  behavior of "MAC"
  it should "pass inLeft to outRight" in {
    val p = new MACParams
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step()
      
      dut.io.outRight.bits.expect(2.S)
      dut.io.outBottom.bits.expect(2.S)
      dut.io.outRight.valid.expect(true.B)
      dut.io.outBottom.valid.expect(true.B)

      dut.io.outResult.bits.expect(4.S)
      dut.io.outResult.valid.expect(true.B)
    }
  }
  it should "not move data without valid input" in {
    val p = new MACParams
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(false.B)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inTop.valid.poke(false.B)
      dut.clock.step()
      dut.io.outBottom.valid.expect(false.B)
      dut.io.outRight.valid.expect(false.B)
      dut.io.outResult.valid.expect(false.B)
    }
  }
  it should "not send outputs unless ready" in {
    // TODO: implement later. figure out ready signals
  }
  it should "do one round of multiplication" in {
    val p = new MACParams
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step()
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(4.S)
    }
  }
  it should "do two rounds of multiplication" in {
    val p = new MACParams
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step()
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(4.S)
      
      dut.io.inLeft.bits.poke(1.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(1.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step()
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(5.S)
      
    }
  }



}