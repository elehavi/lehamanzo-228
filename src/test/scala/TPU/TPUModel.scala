package tpu

import scala.collection.mutable.ArrayBuffer

object MatMulModel {
  // Taken from Ella's HW4
  type Matrix = Seq[Seq[Int]]
  def findValAt(p: TPUParams, a: Matrix, b: Matrix, ai: Int, bj: Int): Int = {
    assert(p.aCols == p.bRows)
    val indices = (0 until p.aCols)
    val toSum =  indices map {k => a(ai)(k) * b(k)(bj)}
    return  toSum.sum
  }

  def apply(p: TPUParams, a: Matrix, b: Matrix): Matrix = {
    assert(a.size == p.aRows)
    assert(a.head.size == p.aCols)
    assert(b.size == p.bRows)
    assert(b.head.size == p.bCols)

    
    val indexed = Seq.fill(p.aRows)(0 until p.bCols)
    val outMat = indexed.zipWithIndex map {case (l, i) => l map {j => findValAt(p,a,b,i,j)}}
    outMat 

  }
}



class fixedTPUModel() {
  type Matrix = Seq[Seq[Int]]

  // format of element of sysarray: (currResult, outRight, outBottom)
  // represent the systolic array
  val sysArray = Array.fill(2)(Array.fill(2)(Array(0,0,0)))

  // process one more cycle of inputs. 
  def progressOneCycle(inTop: Seq[Int], inLeft: Seq[Int]): Unit = {
    // for each row and column, if you are on the top/left edge, take input from module
    // else, take input from previous mac.
    // multiply and accumulate.
    for (i <- Seq(1,0)) { 
      for (j <- Seq(1,0)) {
        var it = 0
        var il = 0
        if(i==0) {
          it = inTop(j)
        } else {
          it = sysArray(i-1)(j)(2)
        }

        if(j==0) {
          il = inLeft(i)
          } else {
            il = sysArray(i)(j-1)(1)
            }
            val currResult = it * il + sysArray(i)(j)(0)
        sysArray(i)(j) = Array(currResult, il, it)
      }
    }
  }
}


class TPUModel(p: TPUParams) {
  type Matrix = Seq[Seq[Int]] 


  // ----------------------------------------------Top/IO Formatting Model----------------------------------------------
  // topVecs and leftVecs represents the vectors containing elements of a/b and 0's that will be passed into the sysArray
  // element n of topVecs/leftVecs will be passed in on cycle n. 
  val topVecs = Array.fill(p.aRows + p.sysW)(Array.fill(p.aCols)(0)) 
  val leftVecs = Array.fill(p.bCols + p.sysW)(Array.fill(p.bRows)(0))
  val cReg = Array.fill(p.aRows)(Array.fill(p.bCols)(0))

  // Takes input Matrices a and b.
  // Fills aReg and bReg with the vectors that will be passed to TPU to compute a x b
  def formatForInput(a: Matrix, b: Matrix): Unit = {
    for (vec <- 0 until (p.aRows + p.sysW)) {
      // (for each column, grab the p.aCols - vec element)
        for (n <- 0 until p.aCols) {
          if (vec-n >= 0 && vec-n < p.aRows){
            topVecs(vec)(n) = a.map{_(n)}(vec-n)
            }
        }
    }

    for (vec <- 0 until (p.bCols + p.sysW)) {
      // same deal here
      for (n <- 0 until p.bRows ) {
          if (vec-n >= 0 && vec-n < p.bCols){ 
            leftVecs(vec)(n) = b(n)(vec-n)
            }
        }
    }
  }

   // ----------------------------------------------Systolic Array Model----------------------------------------------
  // format of element of sysarray: (currResult, outRight, outBottom)
  // represent the systolic array
  val sysArray = Array.fill(p.sysW)(Array.fill(p.sysW)(Array(0,0,0)))
  var cycleCount = 0

  def progressOneCycle(): Unit = {
    // for every mac in the systolic array...   
    for (i <- (p.sysW-1) to 0 by -1) { 
      for (j <- (p.sysW-1) to 0 by -1) {
        var it = 0 // inTop, will be passed out to bottom
        var il = 0 // inLeft, will be passed out to right

        // if we are in the top row, take input from IO.
        if(i==0) {
          it = topVecs(cycleCount)(j)
        } else { // otherwise, grab from the MAC above the current one
          it = sysArray(i-1)(j)(2)
        }

        // same thing for inputs from the left.
        if(j==0) {
          il = leftVecs(cycleCount)(i)
          } else {
            il = sysArray(i)(j-1)(1)
            }
        
        // calculate the product and add it to the running sum
        val currResult = it * il + sysArray(i)(j)(0)
        // update the running sum, outright, and outbottom.
        sysArray(i)(j) = Array(currResult, il, it)
      }
    }
    cycleCount += 1
  }

  // Matrix comes out mirrored along diagonal.
  // This function mirrors it back and puts the result matrix into cReg
  // (technically not transposition since its accross the secondary diagonal)
  def transpose() : Unit = {
    for (i <- 0 until p.sysW) { 
      for (j <- 0 until p.sysW - i) {
        cReg(i)(j) = sysArray(p.sysW - 1 - j)(p.sysW -1 -i)(0)
        cReg(p.sysW - 1 - j)(p.sysW -1 -i) = sysArray(i)(j)(0)
      }
    }
  }

}