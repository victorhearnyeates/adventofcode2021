package dayfour

import scala.annotation.tailrec
import scala.language.implicitConversions

object BoardParser {

  private val pattern = "(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r
  def parseRow(string: String): Vector[Int] = string.trim match {
    case pattern(n1, n2, n3, n4, n5) => Vector(n1.toInt, n2.toInt, n3.toInt, n4.toInt, n5.toInt)
    case _ => Vector.empty
  }

  def parseToBoards(input: Vector[String]): List[Board] = {
    input.map(BoardParser.parseRow).filter(_.nonEmpty).grouped(5).toList.map(Board.apply)
  }

  def parseBoard(input: Vector[String]) = {
    input.map(BoardParser.parseRow).zipWithIndex.flatMap{
      case(row,y) => row.zipWithIndex.map {
        case(value, x) => (x, y) -> value
      }
    }.toMap
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

  def lastBingoBoard(boards: List[Board], numbers: List[Int], acc: List[Board] = Nil): Board = numbers match {
    case Nil => boards.head
    case h :: t => {
      val newBoards = boards.map(_.checkNumber(h))
      val bingoBoards = go(newBoards)
      if (bingoBoards.isEmpty && boards.length == 1) {
        println(h)
        newBoards.head
      } else lastBingoBoard(newBoards.diff(bingoBoards), t, acc)
    }
  }

  @tailrec
  private def go(boards: List[Board], acc: List[Board] = Nil): List[Board] = boards match {
    case Nil => acc
    case h :: t => if (h.isBingo()) go(t, h :: acc) else go(t, acc)
  }
}
