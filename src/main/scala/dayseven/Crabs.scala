package dayseven

import scala.annotation.tailrec

object Crabs {

  def findLowestPower(list: List[Int]): (Int, Int) = {
    val allPowers = list.sorted
    val allDistances = allPowers.map(power => power -> allPowers.map(distance => math.abs(power - distance)).sum)
    allDistances.minBy(_._2)
  }

  def findLowestPower2(list: List[Int]): (Int, Int) = {
    val allPowersList = list.sorted
    val allPowers = (allPowersList.head to allPowersList.last).toList
    val allDistances = allPowers.map(power => power -> allPowersList.map{
      distance => go(createList(math.abs(power - distance)))
    }.sum)
    allDistances.minBy(_._2)
  }

  @tailrec
  def go(list: List[Int], acc: Int = 0): Int = list match {
    case Nil => acc
    case h :: t => go(t, h + acc)
  }

  def createList(int: Int): List[Int] = if (int == 0) List(0) else (0 to int).toList
}
