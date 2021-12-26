package dayfour

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class BingoSpec extends AnyFlatSpec with Matchers {

  val numbers = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

  val boards =
    """22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7
      |""".stripMargin

  "parseRow" should "parse a string into a row" in {
    val rowString = BoardParser.parseRow("22 13 17 11  0")
    val expected = Vector(22, 13, 17, 11, 0)

    rowString shouldBe expected
  }

  it should "parse boards" in {
    val inLines = boards.split("\n").toVector.map(_.trim())
    val board = BoardParser.parseToBoards(inLines)
    board.length shouldBe 3
    board(2).rows.last shouldBe Vector(2, 0, 12, 3, 7)
  }

  "bingoBoard" should "return the bingo board if there's already one" in {
    val boards =
      """22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  3
        | 1  5  1  1  1
        |18  8 23 26  3
        |22 11 13  6  2
        | 2  0 12  3  3
        |""".stripMargin

    val inLines = boards.split("\n").toVector.map(_.trim())
    val board = BoardParser.parseToBoards(inLines)

    val expected = Board(Vector(Vector(14, 21, 17, 24, -1), Vector(-1, 5, -1, -1, -1), Vector(18, 8, 23, 26, -1), Vector(22, 11, 13, 6, -1), Vector(-1, 0, 12, -1, -1)))

    BoardParser.bingoBoard(board, List(1,2,3,4)) shouldBe expected
  }

  it should "give the board in the first problem" in {
    val numbers = NumberParser.parser(ProblemUtils.numbers.mkString(""))
    val boards = BoardParser.parseToBoards(ProblemUtils.boards)

    val expected = Board(
      Vector(
        Vector(58, 43, 51, 49, 56),
        Vector(93, 72, 61, 19, 74),
        Vector(81, 44, -1, 25, 12),
        Vector(-1, -1, -1, -1, -1),
        Vector(94, -1, 63, 52, 8)
      ))

    BoardParser.bingoBoard(boards, numbers) shouldBe expected
  }

  it should "give the sum in the first problem" in {
    val numbers = NumberParser.parser(ProblemUtils.numbers.mkString(""))
    val boards = BoardParser.parseToBoards(ProblemUtils.boards)
    val bingoBoard = BoardParser.bingoBoard(boards, numbers)

    bingoBoard.sum() shouldBe 911
  }

  "lastBingoBoard" should "return the last bingo board" in {
    val boards =
      """22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 1  4  2 2
        | 2 1 3 7  5
        | 9  1  7 5 3
        | 2 1 0 4  4
        | 4 1 6 2  6
        |
        |4 1 7 4  3
        | 1 5 1  1  1
        |1  4 3 2  3
        |2 1 3  6  2
        | 2  0 1  3  3
        |""".stripMargin

    val inLines = boards.split("\n").toVector.map(_.trim())
    val board = BoardParser.parseToBoards(inLines)

    val expected = Board(Vector(Vector(22, 13, 17, 11, 0), Vector(-1, -1, 23, -1, 24), Vector(21, -1, 14, 16, -1), Vector(-1, -1, -1, 18, -1), Vector(-1, 12, 20, 15, 19)))

    BoardParser.lastBingoBoard(board, List(1,2,3,4,5,6,7,8,9,10)) shouldBe expected
  }

  it should "give the sum in the second problem" in {
    val numbers = NumberParser.parser(ProblemUtils.numbers.mkString(""))
    val boards = BoardParser.parseToBoards(ProblemUtils.boards)
    val bingoBoard = BoardParser.lastBingoBoard(boards, numbers)

    bingoBoard.sum() shouldBe 911
  }
}
