package day15

trait Priority[A] {

  def isEmpty: Boolean
  def insert(a: A, priority: Int)
  def extractHighestPriority(): Priority[A]
}
