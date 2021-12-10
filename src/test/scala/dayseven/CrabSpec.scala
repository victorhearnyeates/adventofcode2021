package dayseven

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.{Parser, ProblemUtils}

class CrabSpec extends AnyFlatSpec with Matchers {

  "findLowestPower" should "return the power that requires the least distance" in {
    val distances = List(16,1,2,0,4,2,7,1,2,14)
    val output = Crabs.findLowestPower(distances)

    output shouldBe (2,37)
  }

  it should "give the first part answer" in {
    val distances: List[Int] = Parser.string2List(ProblemUtils.crabs).map(_.toInt)

    Crabs.findLowestPower(distances) shouldBe (361,364898)
  }

  "findLowestPower2" should "return the power that requires the least distance" in {
    val distances = List(16,1,2,0,4,2,7,1,2,14)
    val output = Crabs.findLowestPower2(distances)

    output shouldBe (5,168)
  }

  it should "give the first part answer" in {
    val distances: List[Int] = Parser.string2List(ProblemUtils.crabs).map(_.toInt)

    Crabs.findLowestPower2(distances) shouldBe (500,104149091)
  }
}
