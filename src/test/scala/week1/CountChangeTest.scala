package week1

import org.scalatest.FunSuite

class CountChangeTest extends FunSuite {
  test("1 only") {
    assert(week1.CountChange.countChange(1, List(5, 2, 7, 1)).equals(1))
  }
  test("Null money") {
    assert(week1.CountChange.countChange(0, List(5, 2, 7, 1)).equals(0))
  }
  test("Exact match") {
    assert(week1.CountChange.countChange(5, List(5, 2, 7, 1)).equals(4))
  }
  test("Empty coins list") {
    assert(week1.CountChange.countChange(5, List()).equals(0))
  }
}
