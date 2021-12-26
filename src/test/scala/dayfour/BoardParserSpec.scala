package dayfour

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardParserSpec extends AnyFlatSpec with Matchers {

  val board = Vector(
    "22 13 17 11  0",
    "8  2 23  4 24",
    "21  9 14 16  7",
    "6 10  3 18  5",
    "1 12 20 15 19",
  )

  "parseBoard" should "parse the board into a Map" in {
    val expected = Map((2,2) -> 14, (4,1) -> 24, (1,2) -> 9, (1,4) -> 12, (0,4) -> 1, (3,0) -> 11, (2,4) -> 20, (3,2) -> 16, (4,3) -> 5, (2,3) -> 3, (1,3) -> 10, (0,3) -> 6, (0,2) -> 21, (3,4) -> 15, (2,1) -> 23, (4,2) -> 7, (4,4) -> 19, (2,0) -> 17, (0,1) -> 8, (1,1) -> 2, (1,0) -> 13, (3,1) -> 4, (0,0) -> 22, (4,0) -> 0, (3,3) -> 18)


    BoardParser.parseBoard(board) shouldBe expected
  }
}
