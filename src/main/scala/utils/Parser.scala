package utils

object Parser {

  def string2List(string: String): List[String] = string.split(",").toList
}