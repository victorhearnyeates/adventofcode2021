import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.implicits._

class DayTwoSpec extends AnyFlatSpec with Matchers {

  val list = List(
    "forward 7",
    "down 8",
    "up 5"
  )

  "Mover#parse" should "take in a number and up/down/forward and parse them into a Movement" in {
    val forward = Mover.parse(list.head)
    val down = Mover.parse(list(1))
    val up = Mover.parse(list(2))

    Right(Forward(7)) shouldBe forward
    Right(Down(8)) shouldBe down
    Right(Up(5)) shouldBe up
  }

  it should "Return an error for something it can't parse" in {
    val errorInput = "Not a number"
    val error = Mover.parse(errorInput)

    Left(s"Could not parse $errorInput to movement type") shouldBe error
  }

  it should "Parse the movements into a list" in {
    val movementsStrings = ProblemUtils.movements
    val movements: Either[String, List[Movement]] = movementsStrings.traverse(Mover.parse)

    assert(movements.isRight)
  }

  it should "get a position from those movements" in {
    val movementsStrings = ProblemUtils.movements
    val position = movementsStrings.traverse(Mover.parse).map(_.map(Movement.asPosition).reduce(_ combine _))

    position shouldBe Position(2105,-807)
  }
}
