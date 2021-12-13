package daythirteen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class PaperSpec extends AnyFlatSpec with Matchers {

  "foldY" should "return the numbers folded" in {
    val input = Set(
      (6, 10), (0, 14), (9, 10), (0, 3), (10, 4), (4, 11), (6, 0),
      (6, 12), (4, 1), (0, 13), (10, 12), (3, 4), (3, 0), (8, 4),
      (1, 10), (2, 14), (8, 10), (9, 0)
    )

    val expected = Set((3,4), (2,0), (4,1), (6,2), (10,4), (9,4), (3,0), (6,4), (0,0), (4,3), (0,3), (6,0), (8,4), (1,4), (10,2), (0,1), (9,0))

    val output = Paper.foldY(input, 7)

    output shouldBe expected
  }

  "foldX" should "return the numbers folded" in {
    val input = Set((3,4), (2,0), (4,1), (6,2), (10,4), (9,4), (3,0), (6,4), (0,0), (4,3), (0,3), (6,0), (8,4), (1,4), (10,2), (0,1), (9,0))

    val expected = Set((3,4), (4,1), (1,4), (0,1), (0,4), (3,0), (2,4), (1,0), (4,3), (0,3), (0,2), (4,2), (4,4), (2,0), (4,0), (0,0))

    val output = Paper.foldX(input, 5)

    output shouldBe expected
  }

  it should "give part one answer" in {
    val input = ProblemUtils.dots.map(str => str.split(",").map(_.toInt)).map(a => (a(0), a(1)))
    input.last shouldBe (211,415)

    val output = Paper.foldX(input.toSet, 655)

    output.size shouldBe 790
  }
  
  "They" should "give the answer to part two when combined" in {
    val input = ProblemUtils.dots.map(str => str.split(",").map(_.toInt)).map(a => (a(0), a(1)))

    val output1 = Paper.foldX(input.toSet, 655)
    val output2 = Paper.foldY(output1, 447)
    val output3 = Paper.foldX(output2, 327)
    val output4 = Paper.foldY(output3, 223)
    val output5 = Paper.foldX(output4, 163)
    val output6 = Paper.foldY(output5, 111)
    val output7 = Paper.foldX(output6, 81)
    val output8 = Paper.foldY(output7, 55)
    val output9 = Paper.foldX(output8, 40)
    val output10 = Paper.foldY(output9, 27)
    val output11 = Paper.foldY(output10, 13)
    val output12 = Paper.foldY(output11, 6)

    val maxX = output12.maxBy(_._1)._1
    val maxY = output12.maxBy(_._2)._2

    maxX shouldBe 39
    maxY shouldBe 5

    val asChars = (0 to maxX).map(x => (0 to maxY).map(y => if (output12.contains((x, y))) "#" else "."))
    val asString = asChars.map(_.mkString("")).mkString("\n")

    val expected =
      """
        |......
        |.#..#.
        |#....#
        |#....#
        |.####.
        |......
        |#####.
        |#....#
        |.....#
        |....#.
        |......
        |#.....
        |#.#...
        |#.#...
        |######
        |......
        |.#.##.
        |#.#..#
        |#.#..#
        |######
        |......
        |##...#
        |#.#..#
        |#..#.#
        |#...##
        |......
        |######
        |..#...
        |..#...
        |######
        |......
        |.#.###
        |#..#.#
        |#....#
        |.####.
        |......
        |.##...
        |#..#..
        |#..#..
        |######""".stripMargin

    asString.replaceAll("\n", "") shouldBe expected.replaceAll("\n", "")

  }
}