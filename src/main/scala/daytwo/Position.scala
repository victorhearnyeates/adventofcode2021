package daytwo

case class Position(horizontal: Int, depth: Int) {
  def combine(other: Position) = Position(this.horizontal + other.horizontal, this.depth + other.depth)
}

object Position {
  def init() = Position(0,0)
}