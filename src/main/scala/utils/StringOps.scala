package utils

object StringOps {

  implicit class StringSyntax(str: String) {
    def stripSplit(string: String): Array[String] = str.trim().split(string)
  }
}
