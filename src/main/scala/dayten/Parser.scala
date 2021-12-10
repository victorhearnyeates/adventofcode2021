package dayten

import scala.annotation.tailrec

object Parser {

  @tailrec
  def lineToParse(line: List[Character], acc: List[Opening] = Nil): Either[Closing, List[Opening]] = line match {
    case Nil => Right(acc)
    case Opening(c) :: t => lineToParse(t, Opening(c) :: acc)
    case Closing(c) :: t => if (acc.head.matches(Closing(c))) lineToParse(t, acc.tail) else Left(Closing(c))
  }

  def points(closing: Closing): Int = closing.char match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  @tailrec
  def points(opening: List[Opening], score: Long = 0L): Long = opening match {
    case Nil => score
    case Opening('(') :: t => points(t, (score * 5) + 1)
    case Opening('[') :: t => points(t, (score * 5) + 2)
    case Opening('{') :: t => points(t, (score * 5) + 3)
    case Opening('<') :: t => points(t, (score * 5) + 4)
  }
}
