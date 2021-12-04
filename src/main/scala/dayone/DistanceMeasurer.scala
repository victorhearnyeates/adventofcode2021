package dayone

object DistanceMeasurer {
  def windowValues(list: List[Int], sliding: Int): List[Int] = {
    list.sliding(sliding).map(_.sum).toList
  }

  def increases(distances: List[Int]) = {
    def go(list: List[Int], acc: Int, last: Int): Int = list match {
      case Nil => acc
      case h :: t if h > last => go(t, acc + 1, h)
      case h :: t => go(t, acc, h)
    }

    // accumulator starts at -1 as first number will add one
    go(distances, -1, -1)
  }

}