import dayfour.{BingoNumbers, BoardUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayFourSpec extends AnyFlatSpec with Matchers {

  val firstBoard = ProblemUtils.boards.take(5)

  "parseToBoard" should "parse lones of strings into  a multidimensional array" in {
    val expected = Array(Array("38", "80", "23", "60", "82", "0"), Array("25", "35", "28", "47", "39", "0"), Array("40", "0", "30", "48", "76", "0"), Array("32", "41", "49", "69", "4", "0"), Array("13", "42", "89", "20", "12", "0"), Array("0", "0", "0", "0", "0", "0"))

    BoardUtils.parseToBoard(firstBoard) shouldBe expected
  }

  it should "ignore the line inbetween" in {
    ProblemUtils.boards.take(6)(5) shouldBe ""
  }

  "parseBoards" should "parse all the boards" in {
    BoardUtils.parseBoards(ProblemUtils.boards).length shouldBe 100
  }

  "updateNumber" should "update both the row and column when a number in include" in {
    val input = Array(Array("38", "80", "23", "60", "82", "0"), Array("25", "35", "28", "47", "39", "0"), Array("40", "0", "30", "48", "76", "0"), Array("32", "41", "49", "69", "4", "0"), Array("13", "42", "89", "20", "12", "0"), Array("0", "0", "0", "0", "0", "0"))
    val updated = BoardUtils.updateNumber(input, "80")

    updated.head shouldBe Array("38", "*80*", "23", "60", "82", "1")
    input(5) shouldBe Array("0", "1", "0", "0", "0", "0")
  }

  "parseNumbers" should "create an array of the bingo numbers" in {
    BingoNumbers.parseNumbers(ProblemUtils.bingoNumbers.head).take(4) shouldBe Array("50","68","2","1")
  }

  "isBingo" should "return true if any of the final column is 5" in {
    val expected = Array(Array("38", "80", "23", "60", "82", "0"), Array("25", "35", "28", "47", "39", "5"), Array("40", "0", "30", "48", "76", "0"), Array("32", "41", "49", "69", "4", "0"), Array("13", "42", "89", "20", "12", "0"), Array("0", "0", "0", "0", "0", "0"))

    BoardUtils.isBingo(expected) shouldBe true
  }

  "Day one solution" should "tell me which board gets bingo first" in {
    val allBoards = BoardUtils.parseBoards(ProblemUtils.boards)

    val allNumbers = BingoNumbers.parseNumbers(ProblemUtils.bingoNumbers.head).toList

    val expected = Array(Array("*38*", "*80*", "*23*", "*60*", "*82*", "8"), Array("*25*", "*35*", "*28*", "*47*", "*39*", "6"), Array("*40*", "*0*", "*30*", "*48*", "*76*", "8"), Array("*32*", "*41*", "*49*", "*69*", "*4*", "8"), Array("*13*", "*42*", "*89*", "*20*", "*12*", "7"), Array("9", "5", "7", "7", "6", "*6*"))

    BoardUtils.processBoards(allBoards, allNumbers) shouldBe expected
  }

  it should "give me an answer" in {
    val expected = Array(Array("*38*", "*80*", "*23*", "*60*", "*82*", "8"), Array("*25*", "*35*", "*28*", "*47*", "*39*", "6"), Array("*40*", "*0*", "*30*", "*48*", "*76*", "8"), Array("*32*", "*41*", "*49*", "*69*", "*4*", "8"), Array("*13*", "*42*", "*89*", "*20*", "*12*", "7"), Array("9", "5", "7", "7", "6", "*6*"))
  }
}
