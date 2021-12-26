package utils

import Coordinates._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoordinatesSpec extends AnyFlatSpec with Matchers {

  "findUpdate" should "update a Map" in {
    val input = Map(
      (1,2) -> 2,
      (1,1) -> 1,
      (0,1) -> 0
    )

    val expected = Map(
      (1,2) -> 2,
      (1,1) -> 3,
      (0,1) -> 0
    )

    input.findUpdate((i: Int) => i == 1, 3) shouldBe expected
  }

}
