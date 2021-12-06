package dayfive

object LinesUtils {

  private val regex = "(\\d+),(\\d+)->(\\d+),(\\d+)".r

  def linesParser(linesTxt: List[String]): List[LineSegment] = {
    linesTxt.map(_.replaceAll(" ", "")).map {
      case regex(n1, n2, n3, n4) => LineSegment(Coordinates(n1.toInt, n2.toInt), Coordinates(n3.toInt, n4.toInt))
    }
  }


}
