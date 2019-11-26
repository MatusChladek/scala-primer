package week4

import org.scalatest.FunSuite
import week4.Huffman._

class HuffmanTest extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val l1 = List('a', 'b', 'a')
  }

  test("find frequency of chars in the List") {
    new TestTrees {
      assert(times(l1).toSet === List(('a', 2), ('b', 1)).toSet)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
}
