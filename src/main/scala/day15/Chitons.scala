package day15

import scala.collection.mutable

object Chitons {

  def dijkstra(positions: Vector[Position]) = (startNode: Position, stopNode: Position) => {
    val distances = mutable.Map[Position, Int]()
    val previous = mutable.Map()
    val remaining = positions
    for {
      node <- positions
      _ = distances.update(node, Int.MaxValue)
    }
  }
}

case class Position(x: Int, y: Int, value: Int)
