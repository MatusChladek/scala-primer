package week6

object nQueen extends App {
  def queens(rows: Int): Unit = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k.equals(0)) Set(List())
      else for {
        queens <- placeQueens(k - 1)
        col <- 0.until(rows)
        if isSafe(col, queens)
      } yield col :: queens
      placeQueens(rows)
    }

    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1).zip(queens)

      queensWithRow.forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }
  }

}
