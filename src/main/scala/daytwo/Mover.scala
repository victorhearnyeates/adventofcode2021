package daytwo

import scala.util.matching.Regex

object Mover {
  private val up = regexBuilder("up")
  private val down = regexBuilder("down")
  private val forward = regexBuilder("forward")

  def parse(movement: String): Movement = movement.trim match {
    case up(number) => Up(number.toInt)
    case down(number) => Down(number.toInt)
    case forward(number) => Forward(number.toInt)
  }

  private def regexBuilder(string: String): Regex = s"$string\\s(\\d+)".r
}
