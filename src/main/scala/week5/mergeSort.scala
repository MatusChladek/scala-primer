package week5

import math.Ordering

object mergeSort extends App {
  def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val k = xs.length / 2
    if (k.equals(0)) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = {
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        }
      }

      val (first, second) = xs.splitAt(k)
      merge(msort(first)(ord), msort(second)(ord))
    }
  }

  val nums = List(-5, -10, 77, 3)
  val fruits = List("apple", "pineapple", "banana")
  println(msort(nums)(Ordering.Int))
  println(msort(fruits)(Ordering.String))
}
