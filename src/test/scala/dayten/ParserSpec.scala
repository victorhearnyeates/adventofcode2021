package dayten

import Character._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class ParserSpec extends AnyFlatSpec with Matchers {

  "lineToParse" should "output any opening char" in {
    val list = List('('.toCharacter(), '['.toCharacter(), '<'.toCharacter(), '{'.toCharacter())
    val expected = "{<[(".map(Opening)
    Parser.lineToParse(list, Nil) shouldBe Right(expected.toList)
  }

  it should "remove a char from the list if the closing on appears" in {
    val list = List('('.toCharacter(), '['.toCharacter(), ']'.toCharacter(), '<'.toCharacter(), '{'.toCharacter())
    val expected = "{<(".map(Opening)
    Parser.lineToParse(list) shouldBe Right(expected.toList)
  }

  it should "return the corrupted character" in {
    val list = List('('.toCharacter(), '['.toCharacter(), ')'.toCharacter(), '<'.toCharacter(), '{'.toCharacter())
    Parser.lineToParse(list) shouldBe Left(Closing(')'))
  }

  it should "return part one" in {
    val inputs = ProblemUtils.corrupted.map(_.strip().map(_.toCharacter()).toList)
    val processed = inputs.map(Parser.lineToParse(_).swap).filter(_.isRight).map(c => Parser.points(c.toOption.get))
    processed.sum shouldBe 339411
  }

  it should "give the right output for part two example" in {
    val example = "<{([{{}}[<[[[<>{}]]]>[]]".map(_.toCharacter())
    val parsed = Parser.lineToParse(example.toList)

    val expected = "[({<".map(Opening)

    parsed.toOption.get shouldBe expected.toList

    val points = Parser.points(parsed.toOption.get)

    points shouldBe 294
  }

  it should "return part two" in {
    val inputs = ProblemUtils.corrupted.map(_.strip().map(_.toCharacter()).toList)
    val processed = inputs.map(Parser.lineToParse(_)).filter(_.isRight).map(_.toOption.get)
    val points = processed.map(Parser.points(_)).sorted
    val index = (points.length / 2)
    points(index) shouldBe 2289754624L
  }
}
