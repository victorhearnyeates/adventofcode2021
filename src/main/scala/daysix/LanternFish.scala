package daysix

import scala.annotation.tailrec

case class LanternFish(timer: Int) {
  def decrementWithNew(): List[LanternFish] = if (timer == 0) List(LanternFish(6), LanternFish(8)) else List(LanternFish(timer - 1))
  def decrement() = LanternFish(timer - 1)

  def sum(other: LanternFish): Int = this.timer + other.timer
}

object LanternFish {

  def processList(list: List[LanternFish]): List[LanternFish] = {
    list.flatMap(_.decrementWithNew())
  }

  def listToMap(list: List[LanternFish]): Map[LanternFish, Long] = {
    val grouped = list.groupBy(i => i)
    grouped.map {
      case (lf, ls) => lf -> ls.length.toLong
    }
  }

  def processMap(map: Map[LanternFish, Long]): Map[LanternFish, Long] = {
    val decremented = map.map{
      case (lf, num) => if (lf.timer == 0) LanternFish(6) -> (num + map.getOrElse(LanternFish(7), 0.toLong))
      else lf.decrement() -> num
    }

    decremented ++ Map(LanternFish(8) -> map.getOrElse(LanternFish(0), 0.toLong))
  }

  @tailrec
  def processBigNumber(map: Map[LanternFish, Long], list: List[Int]): Map[LanternFish, Long] = list match {
    case Nil => map
    case _ :: t => processBigNumber(processMap(map), t)
  }
}