package daynine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class HeightSpec extends AnyFlatSpec with Matchers{

  val example = Vector(
    Vector(2,1,9,9,9,4,3,2,1,0),
    Vector(3,9,8,7,8,9,4,9,2,1),
    Vector(9,8,5,6,7,8,9,8,9,2),
    Vector(8,7,6,7,8,9,6,7,8,9),
    Vector(9,8,9,9,9,6,5,6,7,8)
  )

  "findLowest" should "find the lowest in the example" in {
    val output = Heights.findLowestPoints(example)
    val added  = Heights.addUp(output)

    added shouldBe 15
  }

  it should "add up the answer" in {
    val input = ProblemUtils.daynine.map(_.split("").map(_.toInt).toVector)
    val output = Heights.findLowestPoints(input)
    val added = Heights.addUp(output)

    added shouldBe 600
  }
}
