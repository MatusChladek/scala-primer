package week5

object HighOrderFunc extends App {
  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => y * y :: squareList(ys)
    }

  def squareListMap(xs: List[Int]): List[Int] =
    xs.map(x => x * x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs => val (first, rest) = xs.span(y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs).map(ys => (ys.head, ys.length))
  }

  val data = List("a", "a", "a", "b", "c", "c", "a")
  println(encode[String](data))
}
