package week2

object CurryProduct extends App {
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(lower_bound: Int, upper_bound: Int): Int = {
    if (lower_bound > upper_bound) zero
    else combine(f(lower_bound), mapReduce(f, combine, zero)(lower_bound + 1, upper_bound))
  }

  def product(f: Int => Int)(lower_bound: Int, upper_bound: Int): Int = {
    mapReduce(f, (x, y) => x * y, 1)(lower_bound, upper_bound)
  }

  //  def product(f: Int => Int)(lower_bound: Int, upper_bound: Int): Int = {
  //    if (lower_bound > upper_bound) {
  //      1
  //    } else {
  //      f(lower_bound) * product(f)(lower_bound + 1, upper_bound)
  //    }
  //  }

  def factorial(n: Int): Int = {
    product(x => x)(1, n)
  }

  println(product(x => x * x)(3, 4))
  println(factorial(4))
}
