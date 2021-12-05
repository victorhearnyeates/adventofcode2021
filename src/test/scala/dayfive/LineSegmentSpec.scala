package dayfive

import dayfive.LineSegment.points
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

  "points" should "return a list of points a LineSegment covers" in {
    val c1 = Coordinates(1,2)
    val c2 = Coordinates(4,2)
    val c3 = Coordinates(1,2)
    val c4 = Coordinates(1,5)

    val l1 = points(LineSegment(c1,c2))
    val l2 = points(LineSegment(c3,c4))

    l1 shouldBe List(Coordinates(1,2),Coordinates(2,2), Coordinates(3,2), Coordinates(4,2))
    l2 shouldBe List(Coordinates(1,2),Coordinates(1,3), Coordinates(1,4), Coordinates(1,5))
  }
}
