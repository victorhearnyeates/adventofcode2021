package day15

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChitonsSpec extends AnyFlatSpec with Matchers {

  "findLowest" should "return the value in the list that is both lowest and has been visited" in {
    val list = Vector(
      Position(1,2,1,1,true),
      Position(1,2,1,2,false),
      Position(1,2,1,3,false),
      Position(1,2,1,4,true)
    )

    Chitons.findLowest(list) shouldBe Position(1,2,1,1,true)
  }
}
