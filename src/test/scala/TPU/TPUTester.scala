package tpu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec


class MACTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MAC"
  it should "pass inLeft to outRight" in {
    val p = new MACParams(8, 32)
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
    val p = new MACParams(8, 32)
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
    val p = new MACParams(8, 32)
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
    val p = new MACParams(8, 32)
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
    val p = new MACParams(8, 32)
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
    val p = new MACParams(8, 32)
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
    val p = new MACParams(8, 32)
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
behavior of "TPU_fixed"
  it should "Multiply the first inputs" in {
    val p = new TPUParams(8, 32, 2, 2, 2)
    test(new TPU_fixed(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.clear.poke(false.B)
      // Ready to Consume from NW MAC
      dut.io.outNW.ready.poke(true.B)
      // Matrix A row 1, element 1
      dut.io.inTopNW.bits.poke(5.S)
      dut.io.inTopNW.valid.poke(true.B)
      // Matrix B col 1, element 1
      dut.io.inLeftNW.bits.poke(1.S)
      dut.io.inLeftNW.valid.poke(true.B)

      dut.clock.step(1)

      // check NW = 1*5
      dut.io.outNW.valid.expect(true.B)
      dut.io.outNW.bits.expect(5.S)

      // Ready to consume from NE and SW MAC
      dut.io.outNE.ready.poke(true.B)
      dut.io.outSW.ready.poke(true.B)
      // Matrix A row 1, element 2
      dut.io.inTopNW.bits.poke(6.S)
      dut.io.inTopNW.valid.poke(true.B)
      // Matrix A row 2, element 1
      dut.io.inTopNE.bits.poke(7.S)
      dut.io.inTopNE.valid.poke(true.B)
      // Matrix B col 1, element 1
      dut.io.inLeftNW.bits.poke(2.S)
      dut.io.inLeftNW.valid.poke(true.B)
      // Matrix B col 2, element 1
      dut.io.inLeftSW.bits.poke(3.S)
      dut.io.inLeftSW.valid.poke(true.B)

      dut.clock.step(1)

      // First cycle Inputs no longer valid
      dut.io.inTopNW.valid.poke(false.B)
      dut.io.inLeftNW.valid.poke(false.B)

      // check NW = 1*5 + 2*6
      dut.io.outNW.valid.expect(true.B)
      dut.io.outNW.bits.expect(17.S)
      // Check that NE = 1*7
      dut.io.outNE.valid.expect(true.B)
      dut.io.outNE.bits.expect(7.S)
      // Check that SW = 5*3
      dut.io.outSW.valid.expect(true.B)
      dut.io.outSW.bits.expect(15.S)

      // Matrix A row 2 element 2
      dut.io.inTopNE.bits.poke(8.S)
      dut.io.inTopNE.valid.poke(true.B)
      // Matrix B col 2 element 2
      dut.io.inLeftSW.bits.poke(4.S)
      dut.io.inLeftSW.valid.poke(true.B)

      dut.clock.step(1)
      // Second cycle inputs no longer valid
      dut.io.inTopNE.valid.poke(false.B)
      dut.io.inLeftSW.valid.poke(false.B)

      // check NW = 1*5 + 2*6
      dut.io.outNW.valid.expect(false.B)
      dut.io.outNW.bits.expect(17.S)

      // Check that NE = 7*1 + 2*8
      dut.io.outNE.valid.expect(true.B)
      dut.io.outNE.bits.expect(23.S)
      // Check that SW = 3*5 + 4*6
      dut.io.outSW.valid.expect(true.B)
      dut.io.outSW.bits.expect(39.S)

      // Check that SE = 3*7
      dut.io.outSE.valid.expect(true.B)
      dut.io.outSE.bits.expect(21.S)

      dut.clock.step(1)

      // Check that SE = 3*7 + 8*4
      dut.io.outSE.valid.expect(true.B)
      dut.io.outSE.bits.expect(53.S) 

    }
  } 
}

class fixedvecTPUTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Top_vec_io"

  it should "output intermediate accumulations in correct cycles" in {
    val p = new TPUParams(8, 32, 2, 2, 2)

    test(new Top_vec_io(p)).withAnnotations(Seq(WriteVcdAnnotation)) {
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
}


class TPUModelTester extends AnyFlatSpec with ChiselScalatestTester {
   behavior of "TPUModel"
  it should "handle one round of inputs" in {
    val p = TPUParams(8, 16, 2, 2, 2)
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
    val p = TPUParams(8, 16, 2, 2, 2)
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
    val p = new TPUParams(8, 32, 2, 2, 2)

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
}



