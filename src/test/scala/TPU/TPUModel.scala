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
  // represents the output/input of overall TPU
  // format: (systolic array, outRight, outBottom)
  type TPUIO = (Matrix, Array[Int], Array[Int])

  // format of element of sysarray: (currResult, outRight, outBottom)
  // represent the systolic array
  val sysArray = Array.fill(2)(Array.fill(2)(Array(0,0,0)))

  // process one more cycle of inputs. 
  def progressOneCycle(inTop: Seq[Int], inLeft: Seq[Int]): Unit = {
    // for each row and column, if you are on the top/left edge, take input from module
    // else, take input from previous mac.
    // multiply and accumulate.
    // printf("inTop: (%d,%d)\n", inTop(0), inTop(1))
    // printf("inLeft: (%d,%d)\n", inLeft(0), inLeft(1))
   
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
        // printf("(%d,%d): inTop %d, inLeft %d, current result %d \n", i,j,it,il,sysArray(i)(j)(0))
        val currResult = it * il + sysArray(i)(j)(0)
        sysArray(i)(j) = Array(currResult, il, it)
      }
    }
  }
}
class TPUModel(p: TPUParams) {
  type Matrix = Seq[Seq[Int]]
  // represents the output/input of overall TPU
  // format: (systolic array, outRight, outBottom)
  type TPUIO = (Matrix, Array[Int], Array[Int])

  // format of element of sysarray: (currResult, outRight, outBottom)
  // represent the systolic array
  val sysArray = Array.fill(p.aRows)(Array.fill(p.bCols)(Array(0,0,0)))

  def progressOneCycle(inTop: Seq[Int], inLeft: Seq[Int]): Unit = {
    // for each row and column, if you are on the top/left edge, take input from module
    // else, take input from previous mac.
    // multiply and accumulate.
    // printf("inTop: (%d,%d)\n", inTop(0), inTop(1))
    // printf("inLeft: (%d,%d)\n", inLeft(0), inLeft(1))
   
    for (i <- (p.aRows-1) to 0 by -1) { 
      for (j <- (p.bCols-1) to 0 by -1) {
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
        // printf("(%d,%d): inTop %d, inLeft %d, current result %d \n", i,j,it,il,sysArray(i)(j)(0))
        val currResult = it * il + sysArray(i)(j)(0)
        sysArray(i)(j) = Array(currResult, il, it)
      }
    }
  }
<<<<<<< HEAD

=======
>>>>>>> 7b45b87080a45e0fb1865f157d4a7f0f5f2868f9
}