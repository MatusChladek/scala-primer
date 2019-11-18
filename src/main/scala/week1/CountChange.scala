package week1

object CountChange extends App {
  def countChange(money: Int, coins: List[Int]): Int = {

    def func(money_current: Int, coins_current: List[Int]): Int = {
      if (coins_current.isEmpty || money_current < 0) {
        0
      } else if (money_current == 0) {
        1
      } else {
        func(money_current - coins_current.head, coins_current) + func(money_current, coins_current.tail)
      }
    }

    if (money.equals(0)) {
      0
    } else {
      func(money, coins)
    }
  }
}
