package week3

object MainList extends App {
  val t = new Cons(3, new Cons(9, new Cons(8, new Nil)))
  println(t.getElementByIndex(7))
}
