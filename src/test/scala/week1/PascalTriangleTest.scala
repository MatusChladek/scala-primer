package week1

import org.scalatest.FunSuite

class PascalTriangleTest extends FunSuite {
  test("check_root_element") {
    assert(week1.PascalTriangle.pascal(0, 0).equals(1))
  }
  test("check_left_edge_element") {
    assert(week1.PascalTriangle.pascal(0, 10).equals(1))
  }
  test("check_right_edge_element") {
    assert(week1.PascalTriangle.pascal(10, 10).equals(1))
  }
  test("check_nontriangle_invalid_input") {
    assert(week1.PascalTriangle.pascal(5, 3).equals(-1))
  }
  test("check_negative_invalid_input") {
    assert(week1.PascalTriangle.pascal(-5, 3).equals(-1))
    assert(week1.PascalTriangle.pascal(5, -3).equals(-1))
    assert(week1.PascalTriangle.pascal(-5, -3).equals(-1))
  }
  test("simple_solutions") {
    assert(week1.PascalTriangle.pascal(1, 2).equals(2))
  }
  test("difficult_solutions") {
    assert(week1.PascalTriangle.pascal(2,700).equals(244650))
  }
}
