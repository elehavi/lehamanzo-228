package tpu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec


class MACTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MAC"
  it should "pass inLeft to outRight" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      
      dut.io.outRight.bits.expect(2.S)
      dut.io.outBottom.bits.expect(2.S)
      dut.io.outRight.valid.expect(true.B)
      dut.io.outBottom.valid.expect(true.B)

      dut.io.outResult.bits.expect(4.S)
      dut.io.outResult.valid.expect(true.B)
    }
  }
  it should "not move data without valid input" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(false.B)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inTop.valid.poke(false.B)
      dut.clock.step(1)
      dut.io.outBottom.valid.expect(false.B)
      dut.io.outRight.valid.expect(false.B)
      dut.io.outResult.valid.expect(false.B)
    }
  }
  it should "not send outputs unless ready" in {
    // TODO: implement later. figure out ready signals as we connect MAC's
  }
  it should "do one round of multiplication" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(4.S)
    }
  }
  it should "do two rounds of multiplication and clear" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(2.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(4.S)
      
      dut.io.inLeft.bits.poke(1.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(1.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(5.S)
      
      // Assert clear
      dut.io.clear.poke(true.B)
      dut.io.inLeft.valid.poke(false.B)
      dut.io.inTop.valid.poke(false.B)
      dut.clock.step(1)

      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(0.S)
    }
  }
it should "do one round of multiplication, clear, and do another round" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(2.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(3.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(6.S)
      
      // Assert clear
      dut.io.clear.poke(true.B)
      dut.io.inLeft.valid.poke(false.B)
      dut.io.inTop.valid.poke(false.B)
      dut.clock.step(1)

      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(0.S)

      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(5.S)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(5.S)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(25.S)
    }
  }
  
    it should "handle the result of maximum size inputs" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val maxSInt = (1 << (p.w - 1)) - 1

      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(maxSInt)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(maxSInt)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(maxSInt*maxSInt)
    }
  }
    it should "handle the result of minimum size inputs" in {
    val p = new MACParams(8)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val minSInt = 0 - (1 << (p.w - 1))

      dut.io.clear.poke(false.B)
      dut.io.inLeft.bits.poke(minSInt)
      dut.io.inLeft.valid.poke(true.B)
      dut.io.inTop.bits.poke(minSInt)
      dut.io.inTop.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(minSInt*minSInt)
    }
  }
}

class fixedTPUModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "fixedTPUModel"
  it should "handle one round of inputs" in {
    val ftpu = new fixedTPUModel()
    ftpu.progressOneCycle(Seq(1,2), Seq(1,2))
    val expected = Array(Array(Array(1,1,1), Array(0,0,2)), Array(Array(0,2,0), Array(0,0,0)))
    for (row <- 0 until 2) {
      for (col <- 0 until 2) {
        for (elt <- 0 until 3) {
          assert(ftpu.sysArray(row)(col)(elt) == expected(row)(col)(elt))
        }
      }
    }
  }
  it should "complete simple matrix multiplication" in {
    val ftpu = new fixedTPUModel()
    ftpu.progressOneCycle(Seq(1,0), Seq(1,0))
    ftpu.progressOneCycle(Seq(1,2), Seq(1,2))
    ftpu.progressOneCycle(Seq(0,1), Seq(0,1))
    val expected = Array(Array(Array(2,0,0), Array(3,1,1)), Array(Array(3,1,1), Array(4,2,2)))
    for (r <- 0 until 2) {
      for (c <- 0 until 2) {
        for (e <- 0 until 3) {
          assert(ftpu.sysArray(r)(c)(e) == expected(r)(c)(e))
        }
      }
    }
    
  }
  
}


class fixedTPUTester extends AnyFlatSpec with ChiselScalatestTester {
  ???
}

/*
class TPUModelTester extends AnyFlatSpec with ChiselScalatestTester {
  ???
}

class TPUTester extends AnyFlatSpec with ChiselScalatestTester {
  ???
}
*/