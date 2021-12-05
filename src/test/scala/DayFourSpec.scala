import dayfour.{BingoNumbers, BoardUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayFourSpec extends AnyFlatSpec with Matchers {

  val firstBoard = ProblemUtils.boards.take(5)

  "parseToBoard" should "parse lines of strings into  a multidimensional array" in {
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

    val expected = Array(Array("24", "*96*", "*27*", "39", "3", "2"), Array("*54*", "*26*", "*12*", "*65*", "*60*", "5"), Array("67", "*62*", "43", "*98*", "14", "2"), Array("15", "95", "*2*", "82", "33", "1"), Array("*64*", "17", "86", "0", "63", "1"), Array("2", "3", "3", "2", "1", "0"))

    BoardUtils.processBoards(allBoards, allNumbers) shouldBe expected
  }

  "dropIndices" should "drop the last array and furthest right number" in {
    val input = Array(Array("24", "*96*", "*27*", "39", "3", "2"), Array("*54*", "*26*", "*12*", "*65*", "*60*", "5"))

    val expected = Array(Array("24", "*96*", "*27*", "39", "3"))

    BoardUtils.dropIndices(input) shouldBe expected
  }

  "unhighlighted" should "give me the total sum of unhighlighted numbers" in {
    val expected = Array(Array("24", "*96*", "*27*", "39", "*3*", "2"), Array("24", "*96*", "*27*", "10", "6", "1"), Array("1","1","1","1","1","1"))

    BingoNumbers.unhighlighted(expected) shouldBe 103
  }

  it should "give me an answer" in {
    val board = Array(Array("24", "*96*", "*27*", "39", "3", "2"), Array("*54*", "*26*", "*12*", "*65*", "*60*", "5"), Array("67", "*62*", "43", "*98*", "14", "2"), Array("15", "95", "*2*", "82", "33", "1"), Array("*64*", "17", "86", "0", "63", "1"), Array("2", "3", "3", "2", "1", "0"))

    val output = BingoNumbers.unhighlighted(board)

    output * 12 shouldBe 6972
  }
}
