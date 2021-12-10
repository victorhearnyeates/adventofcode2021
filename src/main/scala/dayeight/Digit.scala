package dayeight

import dayeight.Digit.easyNumbers
import utils.StringOps._

case class Digit(segments: String) {
  def asNumber: Int = easyNumbers(segments)
}

object Digit {
  def easyNumbers(segments: String): Int = segments.length match {
    case 2 => 1
    case 3 => 7
    case 4 => 4
    case 7 => 8
    case _ => -1
  }

  def parse(string: String): (List[SignalPattern], List[OutputValue]) = string.stripSplit("\\|") match {
    case Array(signals, ouput) =>
      (signals.stripSplit(" ").map(SignalPattern).toList, ouput.stripSplit(" ").map(OutputValue).toList)
  }
}