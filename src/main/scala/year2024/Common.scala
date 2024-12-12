package year2024

import scala.collection.mutable

type Point = (Int, Int)
extension (a: Point)
  def +(b: Point): Point = (a._1 + b._1, a._2 + b._2)
  def isAdjacentTo(b: Point): Boolean =
    (math.abs(a._1 - b._1) == 1 && a._2 == b._2)
      || (a._1 == b._1 && math.abs(a._2 - b._2) == 1)

case class Position(i: Int, j: Int)
