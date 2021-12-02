package daytwo

sealed trait Movement {
  def distance: Int
}

object Movement {
  def asPosition(movement: Movement): Position = movement match {
    case Up(d) => Position(0,-d) // Depth is measured positively
    case Down(d) => Position(0, d)
    case Forward(d) => Position(d, 0)
  }

  def move(currentPosition: Position, aim: Int, movements: List[Movement]): Position = movements match {
    case Nil => currentPosition
    case Forward(d) :: tail => move(currentPosition combine Position(d, d * aim), aim, tail)
    case Up(d) :: tail => move(currentPosition, aim - d, tail)
    case Down(d) :: tail => move(currentPosition, aim + d, tail)
  }
}

case class Up(distance: Int) extends Movement
case class Down(distance: Int) extends Movement
case class Forward(distance: Int) extends Movement