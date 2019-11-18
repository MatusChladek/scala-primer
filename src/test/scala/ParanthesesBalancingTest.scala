import org.scalatest.FunSuite

class ParanthesesBalancingTest extends FunSuite{
  test("simple_test"){
    assert(week1.ParanthesesBalancing.balance(("Simple test ()".toList)).equals(true))
    assert(week1.ParanthesesBalancing.balance((")(".toList)).equals(false))
    assert(week1.ParanthesesBalancing.balance(("".toList)).equals(true))
  }
}
