import daytwo.{Down, Forward, Movement, Mover, Position, Up}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayTwoSpec extends AnyFlatSpec with Matchers {

  val list = List(
    "forward 7",
    "down 80",
    "up 5"
  )

  "Mover#parse" should "take in a number and up/down/forward and parse them into a Movement" in {
    val forward = Mover.parse(list.head)
    val down = Mover.parse(list(1))
    val up = Mover.parse(list(2))

    Forward(7) shouldBe forward
    Down(80) shouldBe down
    Up(5) shouldBe up
  }

  "Movement" should "change to a position" in {
    Movement.asPosition(Forward(7)) shouldBe Position(7,0)
    Movement.asPosition(Down(80)) shouldBe Position(0,80)
    Movement.asPosition(Up(5)) shouldBe Position(0,-5)
  }

  it should "be able to move things, accounting for aim" in {
    val position = Movement.move(Position.init(), 0, list.map(Mover.parse) :+ Forward(10))
    val expected = Position(17, 750)

    Movement.move(Position.init(), 0, List(Forward(10))) shouldBe Position(10, 0)
    Movement.move(Position.init(), 0, List(Forward(10), Down(1))) shouldBe Position(10, 0)
    Movement.move(Position.init(), 1, List(Forward(10))) shouldBe Position(10, 10)
    Movement.move(Position.init(), 1, List(Up(1), Forward(10))) shouldBe Position(10, 0)
    position shouldBe expected
  }

  it should "be able to calculate the total daytwo" in {
    val movements = ProblemUtils.movements.map(Mover.parse)
    val position = Movement.move(Position.init(), 0, movements)

    position shouldBe Position(2105,757618)
  }

  "Position" should "combine" in {
    val combine = Position(7,0) combine Position(8, -10) combine Position(0,9)

    combine shouldBe Position(15, -1)
  }

  it should "get a position from those movements" in {
    val movementsStrings = ProblemUtils.movements
    val position = movementsStrings.map(Mover.parse).map(Movement.asPosition)

    position.reduce(_ combine _) shouldBe Position(2105,807)
  }
}
