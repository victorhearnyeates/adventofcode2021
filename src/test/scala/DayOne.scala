import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayOne extends AnyFlatSpec with Matchers {

  "DistanceMeasurer" should "keep count of distance increases" in {
    val distances = List(1,2,3,4)
    val expected = 3

    DistanceMeasurer.increases(distances) shouldBe expected
  }

  it should "be zero if all numbers decrease" in {
    val distances = List(4,3,2,1)

    DistanceMeasurer.increases(distances) shouldBe 0
  }

  it should "return 2 for varying increases/decreases" in {
    val distances = List(1,2,1,3)

    DistanceMeasurer.increases(distances) shouldBe 2
  }

  it should "count up all the distances" in {
    DistanceMeasurer.increases(ProblemUtils.distances) shouldBe 1766
  }

  "Sliding windows" should "add up a window f elements in a list" in {
    val list = List(1,2,3,4,5,6,7,8)
    val expected2 = List(3,5,7,9,11,13,15)
    val expected3 = List(6,9,12,15,18,21)

    DistanceMeasurer.windowValues(list, 2) shouldBe expected2
    DistanceMeasurer.windowValues(list, 3) shouldBe expected3
  }

  it should "created the sliding window for the AoC problem" in {
    val distances = DistanceMeasurer.windowValues(ProblemUtils.distances, 3)
    val increases = DistanceMeasurer.increases(distances)

    increases shouldBe 1797
  }
}
