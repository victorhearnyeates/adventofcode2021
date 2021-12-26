package utils

object Coordinates {

  implicit class MapOps[A](map: Map[(Int, Int), A]) {
    def findUpdate(f: A => Boolean, value: A): Map[(Int, Int), A] = {
      map.keys.flatMap(key => if (f(map(key))) map.updated(key, value) else map).toMap
    }
  }
}
