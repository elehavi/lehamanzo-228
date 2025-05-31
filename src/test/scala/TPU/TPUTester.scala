package tpu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random


class MACTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MAC"
  it should "pass inLeft to outRight" in {
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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

      dut.io.outRight.ready.poke(true.B)
      dut.io.outBottom.ready.poke(true.B)

      dut.clock.step(1)
      dut.io.outResult.valid.expect(true.B)
      dut.io.outResult.bits.expect(5.S)
      
      // Assert clear
      dut.io.clear.poke(true.B)
      dut.io.inLeft.valid.poke(false.B)
      dut.io.inTop.valid.poke(false.B)
      dut.clock.step(1)

      dut.io.outResult.valid.expect(false.B)
      dut.io.outResult.bits.expect(0.S)
    }
  }
it should "do one round of multiplication, clear, and do another round" in {
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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

      dut.io.outResult.valid.expect(false.B)
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
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val maxSInt = (1 << (p.iw - 1)) - 1

      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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
    val p = new macParams(8, 32)
    test(new MAC(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val minSInt = 0 - (1 << (p.iw - 1))

      dut.io.clear.poke(false.B)
      dut.io.outResult.ready.poke(true.B)
      dut.io.outResult.ready.poke(true.B)
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

class TPUModelTester extends AnyFlatSpec with ChiselScalatestTester {
   behavior of "TPUModel"
  it should "handle one round of inputs" in {
    val p = TPUParams(8, 16, 2, 2, 2, 2)
    val tpu = new TPUModel(p)
    tpu.progressOneCycle(Seq(1,2), Seq(1,2))
    val expected = Array(Array(Array(1,1,1), Array(0,0,2)), Array(Array(0,2,0), Array(0,0,0)))
    for (row <- 0 until 2) {
      for (col <- 0 until 2) {
        for (elt <- 0 until 3) {
          assert(tpu.sysArray(row)(col)(elt) == expected(row)(col)(elt))
        }
      }
    }
  }
  it should "complete simple matrix multiplication" in {
    val p = TPUParams(8, 16, 2, 2, 2, 2)
    val tpu = new TPUModel(p)
    tpu.progressOneCycle(Seq(1,0), Seq(1,0))
    tpu.progressOneCycle(Seq(1,2), Seq(1,2))
    tpu.progressOneCycle(Seq(0,1), Seq(0,1))
    val expected = Array(Array(Array(2,0,0), Array(3,1,1)), Array(Array(3,1,1), Array(4,2,2)))
    for (r <- 0 until 2) {
      for (c <- 0 until 2) {
        for (e <- 0 until 3) {
          assert(tpu.sysArray(r)(c)(e) == expected(r)(c)(e))
        }
      }
    }
    
  }
}


class parameterizedTPUTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Top_parameterized"

  it should "output intermediate accumulations in correct cycles" in {
    val p = new TPUParams(8, 32, 2, 2, 2, 2)

    test(new Top_parameterized(p)).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut =>
      /* ---------------- poke matrices ---------------- */
      val aInit = Array(Array(1, 2), Array(3, 4))
      val bInit = Array(Array(5, 7), Array(6, 8))

      for (r <- 0 until p.aRows; c <- 0 until p.aCols)
        dut.io.in.bits.a(r)(c).poke(aInit(r)(c).S(p.iw.W))
      for (r <- 0 until p.bRows; c <- 0 until p.bCols)
        dut.io.in.bits.b(r)(c).poke(bInit(r)(c).S(p.iw.W))

      /* ------------- fire the bundle once ------------ */
      dut.io.clear.poke(false.B)
      dut.io.in.valid.poke(true.B)
      dut.clock.step()            // cycle 0
      dut.io.in.valid.poke(false.B)

      /* ------------- pipeline latency ---------------- */
      dut.clock.step(2)           // cycles 1–2

      // NW = 1 × 5
      dut.io.out.bits(0)(0).expect(5.S)

      dut.clock.step()            // cycle 3

      // NW = 1·5 + 2·6  = 17
      dut.io.out.bits(0)(0).expect(17.S)
      // NE = 1 × 7
      dut.io.out.bits(0)(1).expect(7.S)
      // SW = 3 × 5
      dut.io.out.bits(1)(0).expect(15.S)

      dut.clock.step()            // cycle 4

      // NW unchanged
      dut.io.out.bits(0)(0).expect(17.S)
      // NE = 7 + 2·8 = 23
      dut.io.out.bits(0)(1).expect(23.S)
      // SW = 15 + 4·6 = 39
      dut.io.out.bits(1)(0).expect(39.S)
      // SE = 3 × 7 = 21
      dut.io.out.bits(1)(1).expect(21.S)

      dut.clock.step()            // cycle 5   ← SE first product just stored

      /* extra cycle for SE’s second accumulation */
      dut.clock.step()            // cycle 6

      // SE = 21 + 4·8 = 53
      dut.io.out.bits(1)(1).expect(53.S)
    }
  }
  it should "multiply a 3x3 matrix" in {
    val p = new TPUParams(8, 32, 3, 3, 3 ,3)

    test(new Top_parameterized(p)).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut =>
      /* ---------------- poke matrices ---------------- */
      val aInit = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
      val bInit = Array(Array(10, 11, 12), Array(13, 14, 15), Array(16, 17, 18))

      val exp = Array( Array( 84, 90, 96), Array(201, 216, 231), Array(318, 342, 366))

      for (r <- 0 until p.aRows; c <- 0 until p.aCols)
        dut.io.in.bits.a(r)(c).poke(aInit(r)(c).S(p.iw.W))
      for (r <- 0 until p.bRows; c <- 0 until p.bCols)
        dut.io.in.bits.b(r)(c).poke(bInit(r)(c).S(p.iw.W))

      /* ------------- fire the bundle once ------------ */
      dut.io.clear.poke(false.B)
      dut.io.in.valid.poke(true.B)
      dut.clock.step()            // cycle 0
      dut.io.in.valid.poke(false.B)

      /* ------------- pipeline latency ---------------- */
      // Max writes + final reads
      dut.clock.step((p.sysW * 2 - 1) +  (p.sysW * 2 - 1))           // cycles 1–2

      for (r <- 0 until p.sysW; c <- 0 until p.sysW) {
        dut.io.out.bits(r)(c).expect(exp(r)(c).S(p.ow.W))
      }
    }
  }
  it should "multiply a 5x5 matrix" in {
    val p = new TPUParams(8, 32, 5, 5, 5 ,5)

    val rng = new Random(42)
    def randomElem(): Int = rng.nextInt(1 << p.iw) - (1 << (p.iw - 1))

    test(new Top_parameterized(p)).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut =>
      /* ---------------- initialize random matrix inputs ---------------- */
      val aInit: Array[Array[Int]] = Array.fill(p.aRows, p.aCols)(randomElem())
      val bInit: Array[Array[Int]] = Array.fill(p.bRows, p.bCols)(randomElem())

      val exp: Array[Array[Int]] = Array.ofDim[Int](p.aRows, p.bCols)
      for (i <- 0 until p.aRows; j <- 0 until p.bCols) {
        var sum = 0
        for (k <- 0 until p.aCols) {
          sum += aInit(i)(k) * bInit(k)(j)
        }
        exp(i)(j) = sum
      }

      /* ---------------- poke matrices ---------------- */
      println("=== A matrix ===")
      for (c <- 0 until p.aCols) {
        for (r <- 0 until p.aRows) {
          dut.io.in.bits.a(r)(c).poke(aInit(r)(c).S(p.iw.W))
          print(f"${aInit(r)(c)}%4d ")
        }
        println()
      }
      println("=== B matrix ===")
      for (c <- 0 until p.bCols) {
        for (r <- 0 until p.bRows) {
          dut.io.in.bits.b(r)(c).poke(bInit(r)(c).S(p.iw.W))
          print(f"${bInit(r)(c)}%4d ")
        }
        println()
      }
            /* ------------- fire the bundle once ------------ */
      dut.io.clear.poke(false.B)
      dut.io.in.valid.poke(true.B)
      dut.clock.step()            // cycle 0
      dut.io.in.valid.poke(false.B)

      /* ------------- pipeline latency ---------------- */
      // Max writes + final reads
      dut.clock.step(p.maxWrites + p.finalReads)           // cycles 1–2

      println("=== C matrix ===")
      for (c <- 0 until p.cCols) {
        for (r <- 0 until p.cRows) {
          dut.io.out.bits(r)(c).expect(exp(r)(c).S(p.ow.W))
          print(f"${exp(r)(c)}%6d ")
        }
        println()
      }
    }
  }
}



