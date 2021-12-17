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

    highest.max shouldBe 5460 // y = 104
  }

  "moveTilEnd" should "compute everything to x or y limit" in {
    val position = Position(0,0,5,10)

    val output = position.moveTilEnd(100, -100)

    val expected = Set(Position(15,54,0,1,(0,0)), Position(15,54,0,-2,(0,0)), Position(15,52,0,-3,(0,0)), Position(15,49,0,-4,(0,0)), Position(15,-116,0,-19,(0,0)), Position(15,40,0,5,(0,0)), Position(15,-23,0,-13,(0,0)), Position(15,-36,0,-14,(0,0)), Position(15,34,0,-7,(0,0)), Position(15,45,0,-5,(0,0)), Position(15,0,0,-11,(0,0)), Position(15,-65,0,-16,(0,0)), Position(15,55,0,0,(0,0)), Position(15,10,0,-10,(0,0)), Position(15,-50,0,-15,(0,0)), Position(12,27,2,7,(0,0)), Position(14,34,1,6,(0,0)), Position(15,52,0,2,(0,0)), Position(15,-81,0,-17,(0,0)), Position(5,10,4,9,(0,0)), Position(15,-98,0,-18,(0,0)), Position(15,19,0,-9,(0,0)), Position(15,45,0,4,(0,0)), Position(15,27,0,-8,(0,0)), Position(15,49,0,3,(0,0)), Position(15,40,0,-6,(0,0)), Position(15,55,0,-1,(0,0)), Position(9,19,3,8,(0,0)), Position(15,-11,0,-12,(0,0)))

    output shouldBe expected
  }

  it should "create positions" in {
    val possibleYs = (-105 to 105).toList
    val possibleXs = (15 to 250).toList
    val targetYs = (-105 to -57).toList
    val targetXs = (206 to 250).toList

    val combinations = possibleXs.flatMap(x => possibleYs.map(y => (x,y))).toSet

    val allPositions = combinations.map{case (x, y) => Position(0, 0, x, y, (x, y))}

    val positions = allPositions.flatMap(_.moveTilEnd(250, -105)).filter{
      position => targetXs.contains(position.x) && targetYs.contains(position.y)
    }.groupBy(_.start)

    positions.size shouldBe 3618 //3627 too high
  }
}
