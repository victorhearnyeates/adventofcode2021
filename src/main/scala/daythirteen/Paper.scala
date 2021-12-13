package daythirteen

import math.abs

object Paper {

  def foldX(set: Set[(Int, Int)], x: Int): Set[(Int, Int)] = {
    set.map(coord => (abs(coord._1 - x) - 1, coord._2))
  }

  def foldY(set: Set[(Int, Int)], y: Int): Set[(Int, Int)] = {
    set.map(coord => (coord._1, if (coord._2 > y) abs(coord._2 - (y * 2)) else coord._2))
  }
}
