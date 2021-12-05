package dayfive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class DayFiveSpec extends AnyFlatSpec with Matchers {

  "linesParser" should "convert the puzzle input into a data class" in {
    val linesTxt = ProblemUtils.lines.take(3)
    val linesParsed = LinesUtils.linesParser(linesTxt)
    val expected = List(LineSegment(Coordinates(692,826),Coordinates(692,915)), LineSegment(Coordinates(914,338),Coordinates(524,728)), LineSegment(Coordinates(77,505),Coordinates(77,912)))

    linesParsed shouldBe expected
  }

  it should "give all the lines" in {
    val linesTxt = ProblemUtils.lines
    val linesParsed = LinesUtils.linesParser(linesTxt)

    linesParsed.length shouldBe 500
  }

  it should "give me a list of all the coordinates that appear more than once" in {
    val linesTxt = ProblemUtils.lines
    val linesParsed = LinesUtils.linesParser(linesTxt)
    val filtered = linesParsed.filterNot(_.isDiagonal)

    val asCoordinates = filtered.flatMap(LineSegment.points)
    val grouped = asCoordinates.groupBy(i => i).filter(_._2.length > 1)
    grouped.size shouldBe 0
  }
}
