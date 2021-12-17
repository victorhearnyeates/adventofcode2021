package dayseventeen

import cats.implicits._
import dayseventeen.Probe.Position
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProbeSpec extends AnyFlatSpec with Matchers {

  "path" should "return 3 for inital velocity 2" in {
    val targetYs = (-10 to -5).toList
    val probe = Probe(Nil, targetYs).path(2)

    probe.get shouldBe 3
  }

  it  should "return 45 for inital velocity 9" in {
    val targetYs = (-10 to -5).toList
    val probe = Probe(Nil, targetYs).path(9)

    probe.get shouldBe 45
  }

  it should "find the correct Y value for the example" in {
    val possibleYs = (0 to 10).toList
    val targetYs = (-10 to -5).toList

    val highest = possibleYs.mapFilter(y => Probe(Nil, targetYs).path(y)).max

    highest shouldBe 45
  }

  it should "find the correct Y value for part 1" in {
    val possibleYs = (0 to 105).toList
    val targetYs = (-105 to -57).toList

    val highest = possibleYs.mapFilter(y => Probe(Nil, targetYs).path(y))

    highest shouldBe Nil // y = 104
  }

  "moveTilEnd" should "compute everything to x or y limit" in {
    val position = Position(0,0,5,10)

    val output = position.moveTilEnd(100, -100)

    val expected = Set(Position(-33,27,-1,-8), Position(-19,49,-5,-4), Position(-6,54,-7,-2), Position(14,34,1,6), Position(-24,-36,5,-14), Position(11,-116,8,-19), Position(-31,-11,3,-12), Position(15,40,0,5), Position(-19,-50,6,-15), Position(-13,52,-6,-3), Position(-6,-81,8,-17), Position(-24,45,-4,-5), Position(-34,10,1,-10), Position(-33,0,2,-11), Position(-28,-23,4,-13), Position(12,27,2,7), Position(2,-98,9,-18), Position(-13,-65,7,-16), Position(14,49,-2,3), Position(-28,40,-3,-6), Position(-34,19,0,-9), Position(9,54,-4,1), Position(12,52,-3,2), Position(15,45,-1,4), Position(5,55,-5,0), Position(5,10,4,9), Position(0,55,-6,-1), Position(-31,34,-2,-7), Position(9,19,3,8))

    output shouldBe expected
  }

  it should "create positions" in {
    val possibleYs = (-105 to 105).toList
    val possibleXs = (15 to 250).toList
    val targetYs = (-105 to -57).toList
    val targetXs = (206 to 250).toList

    val combinations = possibleXs.flatMap(x => possibleYs.map(y => (x,y))).toSet

    val allPositions = combinations.map{case (x, y) => Position(0, 0, x, y)}

    val positions = allPositions.flatMap(_.moveTilEnd(250, -105)).filter{
      position => targetXs.contains(position.x) && targetYs.contains(position.y)
    }

    positions.size shouldBe 0 //3627 too high
  }
}
