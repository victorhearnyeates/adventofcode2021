import scala.io.Source

object ProblemUtils {

  lazy val distances = Source.fromResource("dayone/distances.txt").getLines().toList.map(_.toInt)
}
