package dayfive

import dayfive.LineSegment.{points, straightPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineSegmentSpec extends AnyFlatSpec with Matchers {

  "isDiagonal" should "return true if the coordinates are diagonal" in {
    val c1 = Coordinates(1,2)
    val c2 = Coordinates(4,3)

    val l1 = LineSegment(c1, c2)

    assert(l1.isDiagonal)
  }

  it should "return false if the coordinates aren't diagonal" in {
    val c1 = Coordinates(1,2)
    val c2 = Coordinates(1,3)

    val l1 = LineSegment(c1, c2)

    assert(!l1.isDiagonal)
  }

  "straightPoints" should "return a list of points a LineSegment covers" in {
    val c1 = Coordinates(1,2)
    val c2 = Coordinates(4,2)
    val c3 = Coordinates(1,2)
    val c4 = Coordinates(1,5)

    val l1 = straightPoints(LineSegment(c1,c2))
    val l2 = straightPoints(LineSegment(c3,c4))

    l1 shouldBe List(Coordinates(1,2),Coordinates(2,2), Coordinates(3,2), Coordinates(4,2))
    l2 shouldBe List(Coordinates(1,2),Coordinates(1,3), Coordinates(1,4), Coordinates(1,5))
  }

  "multiplier" should "return -1 for 4,3" in {
    LineSegment.multipler(4,3) shouldBe -1
  }

  it should "return 1 for 3,4" in {
    LineSegment.multipler(3,4) shouldBe 1
  }

  "numbers2Range" should "return 1,2,3,4 for 1,4" in {
    val nums = LineSegment.numbers2Range(1,4)
    nums shouldBe List(1,2,3,4)
  }

  it should "return 4,3,2,1 for 4,1" in {
    val nums = LineSegment.numbers2Range(4,1)
    nums shouldBe List(1,2,3,4).reverse
  }


  it should "return 1 for 1,1" in {
    val nums = LineSegment.numbers2Range(1,1)
    nums shouldBe List(1)
  }

  "points" should "return all the straight points" in {
    val c1 = Coordinates(1,2)
    val c2 = Coordinates(4,2)
    val c3 = Coordinates(1,2)
    val c4 = Coordinates(1,5)

    val l1 = points(LineSegment(c1,c2))
    val l2 = points(LineSegment(c3,c4))

    l1 shouldBe List(Coordinates(1,2),Coordinates(2,2), Coordinates(3,2), Coordinates(4,2))
    l2 shouldBe List(Coordinates(1,2),Coordinates(1,3), Coordinates(1,4), Coordinates(1,5))
  }

  it should "return all diagonal lines as well" in {
    val c1 = Coordinates(1,1)
    val c2 = Coordinates(3,3)
    val c3 = Coordinates(9,7)
    val c4 = Coordinates(7,9)

    val l1 = points(LineSegment(c1,c2))
    val l2 = points(LineSegment(c3,c4))

    l1 shouldBe List(Coordinates(1,1),Coordinates(2,2), Coordinates(3,3))
    l2 shouldBe List(Coordinates(9,7),Coordinates(8,8), Coordinates(7,9))
  }
}
