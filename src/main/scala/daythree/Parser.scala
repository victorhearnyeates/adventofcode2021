package daythree

import scala.annotation.tailrec

object Parser {
  @tailrec
  def lastRemaining(lists: Array[Array[Int]], comparison: Array[Int], currentPosition: Int = 0): Array[Int] = {
    val remaining = lists.filter(list => list(currentPosition) == comparison(currentPosition))
    if (remaining.length == 1) remaining.head else lastRemaining(remaining, comparison, currentPosition + 1)
  }

  @tailrec
  def lastRemaining2(lists: Array[Array[Int]], pos: Int = 0): Array[Int] = {
    val transposed = lists.transpose
    val array = transposed(pos)
    val mostCommon = Parser.mostCommonOrReplace(array, 1)
    val remaining = lists.filter(list => list(pos) == mostCommon)
    if (remaining.length == 1) remaining.head else lastRemaining2(remaining, pos + 1)
  }

  @tailrec
  def lastRemaining3(lists: Array[Array[Int]], pos: Int = 0): Array[Int] = {
    val transposed = lists.transpose
    val array = transposed(pos)
    val mostCommon = Parser.mostCommonOrReplace2(array, 0)
    val remaining = lists.filter(list => list(pos) == mostCommon)
    if (remaining.length == 1) remaining.head else lastRemaining3(remaining, pos + 1)
  }

  def mostCommon(array: Array[Int]): Int = {
    array.groupBy(i => i).mapValues(_.length).maxBy(_._2)._1
  }

  def mostCommonOrReplace(array: Array[Int], replacement: Int): Int = {
    val mostCommon = array.groupBy(i => i).mapValues(_.length)
    if (mostCommon.getOrElse(1, 0) == mostCommon.getOrElse(0, 0)) replacement else mostCommon.maxBy(_._2)._1
  }

  def mostCommonOrReplace2(array: Array[Int], replacement: Int): Int = {
    val mostCommon = array.groupBy(i => i).mapValues(_.length)
    if (mostCommon.getOrElse(1, 0) == mostCommon.getOrElse(0, 0)) replacement else mostCommon.minBy(_._2)._1
  }

  def parse(string: String): Array[Int] = {
    string.trim.split("").map(_.toInt)
  }

  def binary2Int(binary: List[Int]): Int = {
    Integer.parseInt(binary.mkString(""), 2)
  }
}
