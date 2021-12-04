import daythree.Parser
import daythree.Parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayThreeSpec extends AnyFlatSpec with Matchers {

  "parse" should "parse a string into a List of numbers" in {
    val string = "11001"
    val output = Parser.parse(string)

    output shouldBe Array(1,1,0,0,1)
  }

  "mostCommon" should "return the most common and least common numbers" in {
    val array = Array(1,1,0,0,1)

    Parser.mostCommon(array) shouldBe 1
  }

  "binary2Int" should "convert 101 to 5" in {
    Parser.binary2Int(List(1,0,1)) shouldBe 5
  }

  it should "return the solution for the input" in {
    val input: List[Array[Int]] = ProblemUtils.binaries.map(Parser.parse)
    val mostCommon = input.transpose.map(x => Parser.mostCommon(x.toArray))
    val leastCommon = mostCommon.map(x => if(x == 1) 0 else 1)

    Parser.binary2Int(mostCommon) shouldBe 654
    Parser.binary2Int(leastCommon) shouldBe 3441
  }

  "mostCommonOrReplace" should "return the most common array" in {
    val array1 = Array(1,1,0,0,1)
    val array2 = Array(1,1,0,0,0)

    Parser.mostCommonOrReplace(array1, 0) shouldBe 1
    Parser.mostCommonOrReplace(array2, 1) shouldBe 0
  }

  it should "replace the value if the two numbers are equally common" in {
    val array1 = Array(1,1,0,0,0,1)

    Parser.mostCommonOrReplace(array1, 0) shouldBe 0
  }

  "lastRemaining" should "return the last remaining" in {
    val arrays = Array(
      Array(1,1,0,0,0,1),
      Array(1,1,1,0,0,1),
      Array(1,1,0,0,0,1),
      Array(1,0,1,0,0,1)
    )

    val replacementValue = Array(1,1,1,0,0,1)

    val array: Array[Int] = Parser.lastRemaining(arrays, replacementValue)

    array shouldBe Array(1,1,1,0,0,1)
  }

  it should "return the solution for the input with 1" in {
    val input = ProblemUtils.binaries.map(Parser.parse).map(_.toArray).toArray
    val mostCommon = input.transpose.map(x => Parser.mostCommonOrReplace(x, 1))
    val output = Parser.lastRemaining(input, mostCommon)

    Parser.binary2Int(output.toList) shouldBe 654
  }

  it should "return the solution for the input with 0" in {
    val input: Array[Array[Int]] = ProblemUtils.binaries.map(Parser.parse).map(_.toArray).toArray

    val leastCommon = input.transpose.map(x => Parser.mostCommonOrReplace2(x, 0))
    //val leastCommon = mostCommon.map(x => if(x == 1) 0 else 1)

    val output = Parser.lastRemaining(input, leastCommon)

    Parser.binary2Int(output.toList) shouldBe 3440
  }

  "lastRemaining2" should "return the last remaining" in {
    val arrays = Array(
      Array(1,1,0,0,0,1),
      Array(1,1,1,0,0,1),
      Array(1,0,0,0,0,1),
      Array(1,0,1,0,0,1)
    )

    val array: Array[Int] = Parser.lastRemaining2(arrays)

    array shouldBe Array(1,1,1,0,0,1)
  }

  it should "Return the problem" in {
    val input = ProblemUtils.binaries.map(Parser.parse).map(_.toArray).toArray
    val output = Parser.lastRemaining2(input)

    Parser.binary2Int(output.toList) shouldBe 1935
  }

  "lastRemaining3" should "Return the problem" in {
    val input = ProblemUtils.binaries.map(Parser.parse).map(_.toArray).toArray
    val output = Parser.lastRemaining3(input)

    Parser.binary2Int(output.toList) shouldBe 3145
  }
}
