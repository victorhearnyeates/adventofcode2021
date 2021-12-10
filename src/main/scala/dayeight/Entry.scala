package dayeight

sealed trait Entry

case class OutputValue(value: String) extends Entry

case class SignalPattern(value: String) extends Entry

object Entry {
  def possibleNines(eight: String, seven: String, four: String) = {
    val concat = seven + four
    val remainingChars = eight.diff(concat)
    val possibles = remainingChars.toCharArray.map(char => (concat + char).distinct)
    possibles.toList.map(_.sorted)
  }

  def zeroThree(zeroSix: List[String], one: String): String = {
    (zeroSix.map(str => str.diff(one)).minBy(_.length) + one).distinct.sorted
  }

  def allNumbers(allStrings: List[String]) = {
    val easyNumbers = allStrings.map(segment => Digit.easyNumbers(segment) -> segment).filter(_._1 != -1).toMap
    val nonEasy = allStrings.map(segment => Digit.easyNumbers(segment) -> segment).filter(_._1 == -1).map(_._2)

    val zeroSixNine = nonEasy.filter(_.length == 6).map(_.sorted)
    val twoThreeFive = nonEasy.filter(_.length == 5).map(_.sorted)

    val nine = possibleNines(easyNumbers(8), easyNumbers(7), easyNumbers(4)).intersect(zeroSixNine).head.sorted
    val zero = zeroThree(zeroSixNine.diff(List(nine)), easyNumbers(1))
    val six = zeroSixNine.diff(List(nine)).diff(List(zero)).head

    val topRightLetter = easyNumbers(1).diff(six)
    val five = nine.diff(topRightLetter)
    val twoThree = twoThreeFive.diff(List(five.sorted))
    val three = zeroThree(twoThree, easyNumbers(1).sorted)
    val two = twoThree.diff(List(three)).head

    val nonEasyMap = Map(
      0 -> zero,
      2 -> two,
      3 -> three,
      5 -> five,
      6 -> six,
      9 -> nine
    )

    easyNumbers.map(x => (x._1, x._2.sorted)) ++ nonEasyMap
  }
}