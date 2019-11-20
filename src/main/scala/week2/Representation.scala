package week2

object Representation {
  type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Creates set of one element
   */
  def singletonSet(elem: Int): FunSet = (x => x == elem)

  /**
   * Returns the union of the two given sets
   */
  def union(s: FunSet, t: FunSet): FunSet = (x => contains(s, x) || contains(t, x))

  /**
   * Returns intersection of the two given sets.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (x => contains(s, x) && contains(t, x))

  /**
   * Returns a set which contains all the elements of the set `s` that are not in the set `t`
   */
  def diff(s: FunSet, t: FunSet): FunSet = (x => contains(s, x) && !contains(t, x))

  /**
   * Returns elements of a set that are accepted by a given predicate p
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = (x => contains(s, x) && p(x))

  /**
   * Tests whether a given predicate is true for all elements of the set
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iterate(a: Int): Boolean = {
      if (contains(s, a) && !p(a)) false
      else if (a > 1000) true
      else iterate(a + 1)
    }

    iterate(-1000)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = (y => exists(s, x => y == f(x)))

  val bound = 1000

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet) {
    println(toString(s))
  }
}
