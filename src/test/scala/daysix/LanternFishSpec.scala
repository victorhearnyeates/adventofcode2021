package daysix

import scala.annotation.tailrec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class LanternFishSpec extends AnyFlatSpec with Matchers {

  "decrement" should "return a lanternfish with a timer lower by 1 if timer > 0" in {
    val lanternFish = LanternFish(5)

    lanternFish.decrementWithNew().head shouldBe LanternFish(4)
  }

  it should "return a two lanternfish with a timers 6 and 8 if timer = 0" in {
    val lanternFish = LanternFish(0)

    lanternFish.decrementWithNew() shouldBe List(LanternFish(6), LanternFish(8))
  }

  "processList" should "process a list of lanternfish" in {
    val numbers = List(2,3,2,0,1)
    val lanternFish = numbers.map(LanternFish.apply)
    val newList = LanternFish.processList(lanternFish)

    val expectedNumbers = List(1,2,1,6,8,0)
    val expectedLanternFish = expectedNumbers.map(LanternFish.apply)

    newList shouldBe expectedLanternFish
  }

  // Problem inputs
  val input = ProblemUtils.lanternFish.flatMap(_.split(",")).map(_.toInt)
  val asLF = input.map(LanternFish.apply)

  it should "gives us our part one result" in {
    val days = Range.inclusive(1,80).toList

    val output = go(asLF, days)

    output.length shouldBe 374994
  }

  "processMap" should "decrement all numbers and add on the same number of 8s to the 6s" in {
    val map = Map(
      LanternFish(0) -> 5.toLong,
      LanternFish(1) -> 5.toLong,
      LanternFish(2) -> 5.toLong,
      LanternFish(3) -> 5.toLong,
      LanternFish(4) -> 5.toLong,
      LanternFish(5) -> 5.toLong,
      LanternFish(6) -> 5.toLong,
      LanternFish(7) -> 5.toLong
    )

    val expected = Map(LanternFish(2) -> 5, LanternFish(5) -> 5, LanternFish(6) -> 10, LanternFish(8) -> 5, LanternFish(3) -> 5, LanternFish(0) -> 5, LanternFish(4) -> 5, LanternFish(1) -> 5)

    LanternFish.processMap(map) shouldBe expected
  }

  "processBigNumber" should "give us the same result as part one" in {
    val days = Range.inclusive(1,80).toList
    val asMap = LanternFish.listToMap(asLF)

    val output = LanternFish.processBigNumber(asMap, days).values.sum

    output shouldBe 374994
  }

  it should "be able to handle bigger numbers" in {
    val days = Range.inclusive(1,256).toList
    val asMap = LanternFish.listToMap(asLF)

    val output = LanternFish.processBigNumber(asMap, days).values.sum

    output shouldBe 1686252324092L
  }

  @tailrec
  private def go(lanternFish: List[LanternFish], days: List[Int]): List[LanternFish] = days match {
    case Nil => lanternFish
    case h :: t => go(LanternFish.processList(lanternFish), t)
  }
}
