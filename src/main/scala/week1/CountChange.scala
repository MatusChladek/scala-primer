package week1

object CountChange extends App {
  /**
   * Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there are 3 ways to give change for 4 if you have coins with denomiation
   * 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
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
