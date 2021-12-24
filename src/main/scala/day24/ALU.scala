package day24

object ALU {

  var w: Int = 0
  var x: Int = 0
  var y: Int = 0
  var z: Int = 0

  def inp(a: Int) = {
    (i: Int) => a
  }

  def add(a: Int, b: Int) = {
    (i: Int) => a + b
  }

  def mul(a: Int, b: Int): Int => Int = {
    (i: Int) => a * b
  }

  def div(a: Int, b: Int) = {
    (i: Int) => a / b
  }

  def mod(a: Int, b: Int) = {
    (i: Int) => a % b
  }

  def eql(a: Int, b: Int) = {
    (i: Int) => if (a == b) 1 else 0
  }
}
