package dayeight

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils
import utils.StringOps._

class DigitSpec extends AnyFlatSpec with Matchers {

  private val example =
    """
      |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin

  "parse" should "parse the example input correctly" in {
    val input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"

    Digit.parse(input)._1 shouldBe List(SignalPattern("be"), SignalPattern("cfbegad"), SignalPattern("cbdgef"), SignalPattern("fgaecd"), SignalPattern("cgeb"), SignalPattern("fdcge"), SignalPattern("agebfd"), SignalPattern("fecdb"), SignalPattern("fabcd"), SignalPattern("edb"))
    Digit.parse(input)._2 shouldBe List(OutputValue("fdgacbe"), OutputValue("cefdb"), OutputValue("cefbgd"), OutputValue("gcbe"))
  }

  "Digit" should "correctly assign easy numbers to the above" in {
    val input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    val outputValue = Digit.parse(input)._2
    val numbers = outputValue.map(value => Digit(value.value).asNumber)

    numbers shouldBe List(8, -1, -1, 4)
  }

  it should "process all examples" in {
    val outputValues = example.stripSplit("\n").toList.filterNot(_ == "").map(Digit.parse).flatMap(_._2)

    val numbers = outputValues.map(value => Digit(value.value)).map(_.asNumber).count(_ != -1)

    numbers shouldBe 26
  }

  it should "give first part answer" in {
    val input = ProblemUtils.digits
    val outputValues = input.filterNot(_ == "").map(Digit.parse).flatMap(_._2)

    val numbers = outputValues.map(value => Digit(value.value)).map(_.asNumber).count(_ != -1)

    numbers shouldBe 261
  }

  "outputNumber" should "return the output numbers together" in {
    val input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    val outputValue = Digit.parse(input)._1
    val numbers = outputValue.map(value => value.value)

    val allNumbers = Entry.allNumbers(numbers).map(x => (x._2, x._1)).toMap
    val outputNumbers = Digit.parse(input)._2.map(_.value.sorted)
    val digits = outputNumbers.map(allNumbers(_))
    digits shouldBe List(8, 3, 9, 4)
  }

  it should "handle the example input" in {
    val example = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    val outputValue = Digit.parse(example)._1
    val numbers = outputValue.map(value => value.value)

    val allNumbers = Entry.allNumbers(numbers).map(x => (x._2, x._1)).toMap
    val outputNumbers = Digit.parse(example)._2.map(_.value.sorted)
    val digits = outputNumbers.map(allNumbers(_))
    digits shouldBe List(5, 3, 5, 3)
  }

  it should "handle example2" in {
    val example = "cd gfcde cdbfage cbdfae edc fedgac gadc feagd febdga gbfec | adbfeg dbfcae facbde gcfde"
    val outputValue = Digit.parse(example)._1
    val numbers = outputValue.map(value => value.value)

    val allNumbers = Entry.allNumbers(numbers).map(x => (x._2, x._1)).toMap
    val outputNumbers = Digit.parse(example)._2.map(_.value.sorted)
    val digits = outputNumbers.map(allNumbers(_))
    digits shouldBe List(6, 0, 0, 3)
  }

  it should "handle the real input" in {
    val digits = ProblemUtils.digits.map(Digit.parse).map{ tup =>
      val outputValue = tup._1
      val numbers = outputValue.map(value => value.value)

      val allNumbers = Entry.allNumbers(numbers).map(x => (x._2.sorted, x._1)).toMap
      val outputNumbers = tup._2.map(_.value.sorted)

      outputNumbers.map(allNumbers(_)).map(_.toString).mkString("").toInt
    }

    digits.sum shouldBe 987553
  }
}
