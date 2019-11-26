package week5

object mergeSort extends App {
  def msort[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] = {
    val k = xs.length / 2
    if (k.equals(0)) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = {
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (lessThan(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        }
      }

      val (first, second) = xs.splitAt(k)
      merge(msort(first)(lessThan), msort(second)(lessThan))
    }
  }

  val nums = List(-5, -10, 77, 3)
  val fruits = List("apple", "pineapple", "banana")
  println(msort(nums)((x: Int, y: Int) => x < y))
  println(msort(fruits)((x: String, y: String) => x.compareTo(y) < 0))
}
