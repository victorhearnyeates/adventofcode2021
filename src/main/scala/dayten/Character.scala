package dayten

sealed trait Character { self =>
  def char: Char
}

case class Opening(char: Char) extends Character {
  def matches(c2: Closing): Boolean = {
    val str: String = char.toString + c2.char
    List("{}", "[]", "<>", "()").contains(str)
  }

  def matching(): Char = char match {
    case '{' => '}'
    case '[' => ']'
    case '<' => '>'
    case '(' => ')'
  }
}
case class Closing(char: Char) extends Character

object Character {
  implicit class CharOps(c: Char) {
    def toCharacter(): Character = c match {
      case '(' | '[' | '{' | '<' => Opening(c)
      case _ => Closing(c)
    }
  }
}