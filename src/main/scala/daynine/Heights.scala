package daynine

object Heights {

  def findLowestPoints(input: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    input.zipWithIndex.map { case (vector, y) =>
      vector.zipWithIndex.map { case (num, x) =>
        if (horizontalCheck(vector, num, x) && verticalCheck(input, num, y, x)) num else -1
      }
    }
  }

  def valueAtIndex(v: Vector[Int], i: Int): Int = v.lift(i).getOrElse(9)

  def valueAtIndexVertical(v: Vector[Vector[Int]], y: Int, x: Int): Int = v.lift(y).map(inner => inner(x)).getOrElse(9)

  def horizontalCheck(v: Vector[Int], num: Int, index: Int): Boolean = {
    val left = valueAtIndex(v, index - 1)
    val right = valueAtIndex(v, index + 1)
    left > num && right > num
  }

  def verticalCheck(v: Vector[Vector[Int]], num: Int, y: Int, x: Int): Boolean = {
    val down = valueAtIndexVertical(v, y - 1, x)
    val up = valueAtIndexVertical(v, y + 1, x)
    down > num && up > num
  }

  def addUp(v: Vector[Vector[Int]]): Int = v.map(vector => vector.filter(_ >= 0)).map((vector => vector.map(_ + 1).sum)).sum
}
