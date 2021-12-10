package utils

import scala.io.Source

object ProblemUtils {

  lazy val distances = Source.fromResource("dayone/distances.txt").getLines().toList.map(_.toInt)

  lazy val movements = Source.fromResource("daytwo/movements.txt").getLines().toList

  lazy val binaries = Source.fromResource("daythree/input.txt").getLines().toList

  lazy val boards = Source.fromResource("dayfour/boards.txt").getLines().toVector

  lazy val numbers = Source.fromResource("dayfour/numbers.txt").getLines().toVector

  lazy val lines = Source.fromResource("dayfive/input.txt").getLines().toList

  lazy val lanternFish = Source.fromResource("daysix/input.txt").getLines().toList

  lazy val crabs = Source.fromResource("dayseven/positions.txt").getLines().toList.mkString("").strip()

  lazy val digits = Source.fromResource("dayeight/input.txt").getLines().toList

  lazy val corrupted = Source.fromResource("dayten/input.txt").getLines().toList
}
