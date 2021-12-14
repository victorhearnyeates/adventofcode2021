package daynine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasinSpec extends AnyFlatSpec with Matchers {

  val example = Vector(
    Vector(2,1,9,9,9,4,3,2,1,0),
    Vector(3,9,8,7,8,9,4,9,2,1),
    Vector(9,8,5,6,7,8,9,8,9,2),
    Vector(8,7,6,7,8,9,6,7,8,9),
    Vector(9,8,9,9,9,6,5,6,7,8)
  )

  "coordinatesDeepest" should "be correct" in {
    val output = Basin.coordinatesDeepest(example)

    output shouldBe Vector((1,0), (9,0), (2,2), (6,4))
  }

  it should "give me the answer for the first input" in {
    Basin.mapBasin(example, Set(0), ValueCoordinates(1, (1,0))) shouldBe Set(0)
  }
}
