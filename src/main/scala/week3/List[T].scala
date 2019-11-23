package week3

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def getElementByIndex(index: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false

  def getElementByIndex(index: Int): T = {
    if (index.equals(0)) {
      this.head
    } else {
      tail.getElementByIndex(index - 1)
    }
  }

  override def toString: String = "{" + head + "}{" + tail + "}"
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: T = throw new NoSuchElementException("Nil.head")

  def tail: List[T] = throw new NoSuchElementException("Nil.tail")

  def getElementByIndex(index: Int): T = throw new IndexOutOfBoundsException("There is not such index")
}

/**
 * Can be passed to fucntions as well
 */
//def singleton[T] (elem: T) = new Cons[T] (elem, new Nil[T] )
//singleton[Int](1)
