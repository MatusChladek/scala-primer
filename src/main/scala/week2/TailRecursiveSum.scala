package week2

object TailRecursiveSum extends App {
  def sum(f: Int => Int, lower_bound: Int, upper_bound: Int): Int = {
    def loop(lower_bound: Int, acc: Int): Int = {
      if (lower_bound > upper_bound) acc
      else loop(lower_bound + 1, f(lower_bound) + acc)
    }

    loop(lower_bound, 0)
  }

  print(sum(x => x * x, 3, 5))
}
