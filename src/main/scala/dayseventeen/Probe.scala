package dayseventeen

import scala.annotation.tailrec

object Probe {

  case class Position(x: Int, y: Int, vX: Int, vY: Int, start: (Int, Int) = (0,0)) {
    def move(): Position = {
      val newVX = if (vX < 0) vX + 1 else if (vX == 0) 0 else vX -1
      Position(x + vX, y + vY, newVX, vY - 1, start)
    }

    def moveTilEnd(xLimit: Int, yLimit: Int, acc: Set[Position] = Set.empty): Set[Position] = {
      if (x > xLimit || y < yLimit) acc
      else {
        val newPos = move()
        newPos.moveTilEnd(xLimit, yLimit, acc + newPos)
      }
    }
  }
}

case class Probe(targetX: List[Int], targetY: List[Int]) {
  @tailrec
  final def path(y: Int, yPos: Int = 0, highest: Int = 0): Option[Int] = {
    if (yPos < targetY.min) None
    else if (yPos <= targetY.max) Some(highest)
    else path(y - 1, yPos + y, math.max(yPos, highest))
  }
}
