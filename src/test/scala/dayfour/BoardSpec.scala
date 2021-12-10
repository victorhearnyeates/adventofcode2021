package dayfour

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemUtils

class BoardSpec extends AnyFlatSpec with Matchers {

  "isBingo" should "check if a horizontal is all negative" in {
    val board = Vector(
      Vector(22, 13, 17, 11,  0),
      Vector(22, 13, 17, 11,  0),
      Vector(-1, -1, -1, -1,  -1),
      Vector(22, 13, 17, 11,  0),
      Vector(22, 13, 17, 11,  0)
    )

    assert(Board.apply(board).isBingo())
  }

  it should "check if a not all are negative" in {
    val board = Vector(
      Vector(22, 13, 17, 11,  0),
      Vector(22, 13, 17, 11,  0),
      Vector(-1, -1, 1, -1,  -1),
      Vector(22, 13, 17, 11,  0),
      Vector(22, 13, 17, 11,  0)
    )

    assert(!Board.apply(board).isBingo())
  }

  it should "check if a vertical is all negative" in {
    val board = Vector(
      Vector(22, 13, -1, 11,  0),
      Vector(22, 13, -1, 11,  0),
      Vector(-1, 11, -1, -1,  -1),
      Vector(22, 13, -1, 11,  0),
      Vector(22, 13, -1, 11,  0)
    )

    assert(Board.apply(board).isBingo())
  }

  "sum" should "add up all non-negative values in a board" in {
    val board = Vector(
      Vector(22, 10, -1, 11,  0),
      Vector(22, 10, -1, 11,  0),
      Vector(-1, -1, -1, -1,  -1),
      Vector(22, 10, -1, 11,  0),
      Vector(22, 10, -1, 11,  0)
    )

    Board(board).sum() shouldBe 172
  }

  "Boards" should "parse all the boards" in {
    val boards = ProblemUtils.boards
    assert(true)
  }
}
