package daynine

object Basin {

  def findBasins(vector: Vector[Vector[Int]]) = {
    val centres = Heights.findLowestPoints(vector).zipWithIndex.flatMap { case (vector, y) =>
      vector.zipWithIndex.map { case (num, x) =>
        if (num >= 0) (x, y) else (-1, -1)
      }
    }.filterNot(tup => tup._1 == -1)
  }


  def valueAtIndex(v: Vector[Int], i: Int): Int = v.lift(i).getOrElse(0)

  def valueAtIndexVertical(v: Vector[Vector[Int]], y: Int, x: Int): Int = v.lift(y).map(inner => inner(x)).getOrElse(0)

  def mapBasin(vector: Vector[Vector[Int]], num: Set[Int], current: ValueCoordinates): Set[Int] = {
    if (current.value == 9) num
    else {
      val left = mapBasin(vector, Set(current.value), ValueCoordinates(valueAtIndex(vector(current.coordinates._2), current.coordinates._1 - 1), (current.coordinates._1, current.coordinates._2 - 1)))
      val right = mapBasin(vector, Set(current.value), ValueCoordinates(valueAtIndex(vector(current.coordinates._2), current.coordinates._1 + 1), (current.coordinates._1, current.coordinates._2 + 1)))
      val up = mapBasin(vector, Set(current.value), ValueCoordinates(valueAtIndexVertical(vector, current.coordinates._2  - 1, current.coordinates._1), (current.coordinates._1 - 1, current.coordinates._2)))
      val down = mapBasin(vector, Set(current.value), ValueCoordinates(valueAtIndexVertical(vector, current.coordinates._2 + 1, current.coordinates._1), (current.coordinates._1 + 1, current.coordinates._2)))

      left ++ right ++ up ++ down
    }
  }

  def coordinatesDeepest(vector: Vector[Vector[Int]]): Vector[(Int, Int)] = {
    Heights.findLowestPoints(vector).zipWithIndex.flatMap { case (vector, y) =>
      vector.zipWithIndex.map { case (num, x) =>
        if (num >= 0) (x, y) else (-1, -1)
      }
    }.filterNot(tup => tup._1 == -1)
  }
}

case class ValueCoordinates(value: Int, coordinates: (Int, Int))