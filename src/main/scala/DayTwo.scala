import scala.util.matching.Regex

object Mover {
  private val up = regexBuilder("up")
  private val down = regexBuilder("down")
  private val forward = regexBuilder("forward")
  def parse(movement: String): Either[String,Movement] =  movement.trim match {
    case up(number) => Right(Up(number.toInt))
    case down(number) => Right(Down(number.toInt))
    case forward(number) => Right(Forward(number.toInt))
    case error => Left(s"Could not parse $error to movement type")
  }

  private def regexBuilder(string: String): Regex = s"$string\\s(\\d+)".r
}

case class Position(horizontal: Int, depth: Int) {
  def combine(other: Position) = Position(this.horizontal + other.horizontal, this.depth + other.depth)
}

sealed trait Movement {
  def distance: Int
}

object Movement {
  def asPosition(movement: Movement): Position = movement match {
    case Up(d) => Position(0,d)
    case Down(d) => Position(0, -d)
    case Forward(d) => Position(d, 0)
  }
}

case class Up(distance: Int) extends Movement
case class Down(distance: Int) extends Movement
case class Forward(distance: Int) extends Movement
