package week1

import scala.util.{Failure, Success, Try}

object PascalTriangle extends App {
  def pascal(column: Int, row: Int): Int = {

    def isValid: Any = {
      if (column > row || column < 0 || row < 0) {
        throw new IllegalArgumentException("Invalid inputs")
      }
    }

    // input validation
    try {
      isValid
    } catch {
      case e: IllegalArgumentException => {
        println(e)
        return -1
      }
    }

    if (column.equals(0) || column.equals(row)) {
      // cover edges
      1
    }
    else {
      pascal(column - 1, row - 1) + pascal(column, row - 1)
    }
  }

  print(pascal(2, 700))

}


