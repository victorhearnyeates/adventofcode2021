package dayfour

case class Board(rows: Vector[Vector[Int]]) {
  def checkNumber(num: Int): Board = Board(rows.map{
    row => row.map(x => if(x == num) -1 else x)
  })

  def isBingo(): Boolean = {
    rows.map(allNegative).reduce(_ || _) || rows.transpose.map(allNegative).reduce(_ || _)
  }

  def sum(): Int = rows.flatMap(_.filter(_ >= 0)).sum

  private def allNegative(row: Vector[Int]): Boolean = row.forall(num => num < 0)
}

object Board {
  def empty() = Board(Vector(Vector()))
}

