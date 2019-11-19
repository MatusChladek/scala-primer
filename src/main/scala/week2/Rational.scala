package week2

object Rational extends App {
  def a = new Rational(5, 6)

  def b = new Rational(1, 6)

  def c = new Rational(1, 3)

  val d = new Rational(2)

  println(a + b)
  println(a.neg())
  println(a.sub(b).sub(c))
  println(d)

}

class Rational(x: Int, y: Int) {
  require(!y.equals(0), "Denominator has to be non-zero")

  def this(x: Int) = this(x, 1)

  def numerator: Int = x

  def denominator: Int = y

  def + (that: Rational): Rational = {
    new Rational(
      x = this.numerator * that.denominator + this.denominator * that.numerator,
      y = this.denominator * that.denominator
    )
  }

  def neg(): Rational = {
    new Rational(
      x = -1 * this.numerator,
      y = this.denominator
    )
  }

  def sub(that: Rational): Rational = {
    this + that.neg()
  }

  override def toString: String = {
    def gcd(a: Int, b: Int): Int = if (b.equals(0)) a else gcd(b, a % b)

    this.numerator / gcd(x, y) + "/" + this.denominator / gcd(x, y)
  }
}