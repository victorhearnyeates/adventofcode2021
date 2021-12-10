package dayfour

object NumberParser {

  def parser(string: String): List[Int] = string.split(",").toList.map(_.toInt)
}
