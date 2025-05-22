


object MatMulModel {
  type Matrix = Seq[Seq[Int]]
  def findValAt(p: MatMulParams, a: Matrix, b: Matrix, ai: Int, bj: Int): Int = {
    assert(p.aCols == p.bRows)
    val indices = (0 until p.aCols)
    val toSum =  indices map {k => a(ai)(k) * b(k)(bj)}
    return  toSum.sum
  }

  def apply(p: MatMulParams, a: Matrix, b: Matrix): Matrix = {
    assert(a.size == p.aRows)
    assert(a.head.size == p.aCols)
    assert(b.size == p.bRows)
    assert(b.head.size == p.bCols)

    
    val indexed = Seq.fill(p.aRows)(0 until p.bCols)
    val outMat = indexed.zipWithIndex map {case (l, i) => l map {j => findValAt(p,a,b,i,j)}}
    outMat 

  }
}