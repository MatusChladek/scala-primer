package week4


object ListInsert {
  def insert(x: Int, xs: List[Int]): List[Int] = {
    xs match {
      case List() => List(x)
      case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
    }
  }

}
