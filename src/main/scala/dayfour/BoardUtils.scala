package dayfour

import scala.util.matching.Regex

object BoardUtils {
  def processBoards(allBoards: Vector[Board], allNumbers: List[String]): Board = allNumbers match {
    case Nil => allBoards.head
    case h :: t =>
      val allUpdated = allBoards.map { board =>
        val (updated, isBingo) = updateCheck(board, h)
        if (isBingo) processBoards(Vector(updated), Nil) else updated
      }
      processBoards(allUpdated, t)
  }

  type Board = Array[Array[String]]

  def parseBoards(boards: List[String]): Vector[Board] = {
    boards.grouped(6).map(list => parseToBoard(list.take(5))).toVector
  }

  def isBingo(board: Board): Boolean = {
    ((0 to 4).map(board(_)(5)) ++ board(5)) contains("5")
  }

  def updateNumber(input: Board, str: String): Board = {
    val newBoard = Array.copyOf(input,6)
    newBoard.zipWithIndex.foreach { case(array, y) =>
      array.zipWithIndex.foreach { case(num, x) =>
        if (num == str && isNotIndex(x, y)) {
          newBoard(5)(x) = update(input(x)(5))
          newBoard(y)(5) = update(input(5)(y))
          newBoard(y)(x) = s"*${newBoard(y)(x)}*"
        }
      }
    }
    newBoard
  }

  def update(str: String) = {
    val number = str.replaceAll("\\*", "").toInt + 1
    if (number > 5) "5" else s"$number"
  }

  def isNotIndex(x: Int, y: Int) = !((x == 5) || (y == 5))

  private def updateCheck(input: Board, str: String): (Board, Boolean) = {
    val updated = updateNumber(input, str)
    val bingo = isBingo(updated)
    (updated, bingo)
  }


  private val pattern: Regex = "(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r
  def parseToBoard(strings: List[String]): Board = {
    val board = Array.fill[String](6,6)("0")
      strings.zipWithIndex.foreach{ case (str, int) => str.strip() match {
        case pattern(n1, n2, n3, n4, n5) =>
          board(int)(0) = n1
          board(int)(1) = n2
          board(int)(2) = n3
          board(int)(3) = n4
          board(int)(4) = n5
          board(int)(5) = "0" // Number found
      }
    }
    board
  }
}
