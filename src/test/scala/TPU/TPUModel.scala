package tpu

import scala.collection.mutable.ArrayBuffer
/* TODO: uncomment when TPU is parametrized and ready for testing

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
*/

// TODO: create parametrized version
class fixedTPUModel {
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
    for (i <- 0 until 2) { 
      for (j <- 0 until 2) {
        var it = 0
        var il = 0
        if(j==0) {
          it = inTop(i)
        } else {
          it = sysArray(j-1)(i)(2)
          }

        if(i==0) {
          it = inLeft(j)
          } else {
            it = sysArray(j)(i-1)(2)
            }
        val currResult = it * il + sysArray(j)(i)(0)
        sysArray(j)(i) = Array(currResult, il, it)
      }
    }
  }


}