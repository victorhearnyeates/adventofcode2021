package dayfive

case class LineSegment(coordinate1: Coordinates, coordinate2: Coordinates) {
  lazy val length: Int = math.abs(math.abs(coordinate1.x) - math.abs(coordinate2.x))
  lazy val isDiagonal = !((coordinate1.x == coordinate2.x) || (coordinate1.y == coordinate2.y))
}

object LineSegment {
  def straightPoints(lineSegment: LineSegment) = {
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

  def points(lineSegment: LineSegment) = {
    val xRange = numbers2Range(lineSegment.coordinate1.x, lineSegment.coordinate2.x)
    val yRange = numbers2Range(lineSegment.coordinate1.y, lineSegment.coordinate2.y)
    if (xRange.length == 1) {
      yRange.map(y => Coordinates(xRange.head, y))
    } else if (yRange.length == 1) {
      xRange.map(x => Coordinates(x, yRange.head))
    } else {
      xRange.zip(yRange).map{case (x,y) => Coordinates(x, y)}
    }
  }

  def multipler(n1: Int, n2: Int): Int = {
    -((n1 - n2.toDouble) / math.abs(n1 - n2.toDouble)).toInt
  }

  def numbers2Range(n1: Int, n2: Int): List[Int] = {
    if (n1 == n2) {
      List(n1)
    } else {
      val increments = multipler(n1, n2)
      (for(i <- (n1 to n2 by increments)) yield i).toList
    }
  }
}
