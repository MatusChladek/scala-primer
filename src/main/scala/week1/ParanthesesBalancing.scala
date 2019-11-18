package week1

import scala.annotation.tailrec

object ParanthesesBalancing extends App {
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def func(chars: List[Char], count_open_parantheses: Int): Boolean = {
      if (chars.isEmpty) {
        count_open_parantheses == 0
      } else if (count_open_parantheses < 0) {
        false
      } else {
        val head = chars.head
        if (head.equals('(')) {
          func(chars.tail, count_open_parantheses + 1)
        } else if (head.equals(')')) {
          func(chars.tail, count_open_parantheses - 1)
        } else {
          func(chars.tail, count_open_parantheses)
        }
      }
    }

    func(chars, 0)
  }
}
