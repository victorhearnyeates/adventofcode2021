package dayfour

import scala.annotation.tailrec
import scala.language.implicitConversions

object BoardParser {

  private val pattern = "(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r
  def parseRow(string: String): Vector[Int] = string match {
    case pattern(n1, n2, n3, n4, n5) => Vector(n1.toInt, n2.toInt, n3.toInt, n4.toInt, n5.toInt)
    case _ => Vector.empty
  }

  def parseToBoards(input: Vector[String]): List[Board] = {
    input.map(BoardParser.parseRow).filter(_.nonEmpty).grouped(5).toList.map(Board.apply)
  }

  @tailrec
  def bingoBoard(boards: List[Board], numbers: List[Int]): Board = numbers match {
    case Nil => boards.head
    case h :: t => {
      val newBoards = boards.map(_.checkNumber(h))
      val board = go(newBoards)
      if (board.length == 1) {
        println(h)
        bingoBoard(board, Nil)
      } else bingoBoard(newBoards, t)
    }
  }

  @tailrec
  private def go(boards: List[Board]): List[Board] = boards match {
    case Nil => Nil
    case h :: t => if (h.isBingo()) List(h) else go(t)
  }
}
