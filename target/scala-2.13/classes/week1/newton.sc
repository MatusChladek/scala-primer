def sqrt(x: Double): Double = {
  def abs(z: Double): Double = {
    if (z < 0) {
      -z
    }
    else {
      z
    }
  }

  def isGoodEnough(guess: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001
  }

  def improve(guess: Double): Double = {
    (guess + x / guess) / 2
  }

  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess)) {
      guess
    }
    else {
      sqrtIter(improve(guess))
    }
  }

  sqrtIter(1.0)
}

sqrt(2)
sqrt(1e60)
sqrt(1e-6)