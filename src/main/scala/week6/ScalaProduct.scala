package week6

object ScalaProduct extends App {
  def ScalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    xs.zip(ys).map(xy => xy._1 * xy._2).sum
  }

  def ScalarProductAlt(xs: Vector[Double], ys: Vector[Double]): Double = {
    xs.zip(ys).map {
      case (x, y) => x * y
    }.sum
  }

  def isPrime(n: Int): Boolean = {
    ((2 until n).forall(d => n % d != 0))
  }

  val x = 17
  println(isPrime(x))

  def ForExpressions(n: Int): IndexedSeq[Tuple2[Int, Int]] = {
    (1.until(n)).flatMap(i =>
      (1.until(i)).map(j => (i, j))).filter(pair => isPrime(pair._1 + pair._2))
  }

  println(ForExpressions(7))
}
