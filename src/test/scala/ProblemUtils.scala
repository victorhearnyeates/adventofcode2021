import scala.io.Source

object ProblemUtils {

  lazy val distances = Source.fromResource("dayone/distances.txt").getLines().toList.map(_.toInt)

  lazy val movements = Source.fromResource("daytwo/movements.txt").getLines().toList

  lazy val binaries = Source.fromResource("daythree/input.txt").getLines().toList

  lazy val boards = Source.fromResource("dayfour/boards.txt").getLines().toList

  lazy val bingoNumbers = Source.fromResource("dayfour/numbers-to-pull.txt").getLines().toList
}
