package week1

object ParanthesesBalancing extends App {
  def balance(chars: List[Char]): Boolean = {
    def func(chars: List[Char], count_open_parantheses: Int): Boolean = {
      if (chars.isEmpty) {
        count_open_parantheses == 0
      } else {
        val head = chars.head
        val remainder =
          if (head.equals('(')) {
            count_open_parantheses + 1
          } else if (head.equals(')')) {
            count_open_parantheses - 1
          } else {
            count_open_parantheses
          }
        if (remainder >= 0) {
          func(chars.tail, remainder)
        } else {
          false
        }
      }
    }

    func(chars, 0)
  }

  print(balance("()".toList))
}
