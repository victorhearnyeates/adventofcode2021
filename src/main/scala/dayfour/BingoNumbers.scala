package dayfour

import dayfour.BoardUtils.Board

object BingoNumbers {
  def unhighlighted(board: Board): Int = {
    val removedIndex = BoardUtils.dropIndices(board)
    val filtered = removedIndex.flatMap{
      array => array.filterNot(_.contains("*"))
    }
    filtered.map(_.toInt).sum
  }

  def parseNumbers(string: String): Array[String] = string.split(",")
}
