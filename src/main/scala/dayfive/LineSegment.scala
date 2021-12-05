package dayfive

case class LineSegment(coordinate1: Coordinates, coordinate2: Coordinates) {
  lazy val length: Int = math.abs(math.abs(coordinate1.x) - math.abs(coordinate2.x))
  lazy val isDiagonal = !((coordinate1.x == coordinate2.x) || (coordinate1.y == coordinate2.y))
}

object LineSegment {
  def points(lineSegment: LineSegment) = {
    val xPoints = Seq(lineSegment.coordinate1.x, lineSegment.coordinate2.x).sorted
    val xRange = Range.inclusive(xPoints.head, xPoints(1)).toList
    val yPoints = Seq(lineSegment.coordinate1.y, lineSegment.coordinate2.y).sorted
    val yRange = Range.inclusive(yPoints.head, yPoints(1)).toList
    if (xRange.length == 1) {
      yRange.map(y => Coordinates(xRange.head, y))
    } else {
      xRange.map(x => Coordinates(x, yRange.head))
    }
  }
}
