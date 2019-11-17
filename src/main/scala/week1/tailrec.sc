import scala.annotation.tailrec

/**
 * Tail recursive version of factorial (ie uses constant space and will not overflow)
 *
 * @return Double
 */

def factorial(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int = {
    if (n.equals(0)) {
      acc
    }
    else {
      loop(acc * n, n - 1)
    }
  }

  loop(1, n)
}


factorial(4)