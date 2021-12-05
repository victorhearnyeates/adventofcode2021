package utils

import scala.io.Source

object ProblemUtils {

  lazy val distances = Source.fromResource("dayone/distances.txt").getLines().toList.map(_.toInt)

  lazy val movements = Source.fromResource("daytwo/movements.txt").getLines().toList

  lazy val binaries = Source.fromResource("daythree/input.txt").getLines().toList

  lazy val lines = Source.fromResource("dayfive/input.txt").getLines().toList
}
