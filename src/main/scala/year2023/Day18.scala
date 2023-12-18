package year2023

import scala.io.Source
import scala.collection.mutable

def day18: Unit = {
  val source = Source.fromFile("resources/2023/day-18a")
  val lines = source.getLines.toVector
  source.close()

  val spec = lines.map {
    case s"$direction $length (#$rgb)" =>
      (direction, length.toInt, rgb)
  }

  val corners = {
    val cornersAux = spec.scanLeft((0, 0)) { case ((i, j), (dir, length, _)) =>
      dir match {
        case "U" => (i - length, j)
        case "D" => (i + length, j)
        case "L" => (i, j - length)
        case "R" => (i, j + length)
      }
    }
    val minI = cornersAux.map(_._1).min
    val minJ = cornersAux.map(_._2).min
    cornersAux.map { (i, j) => (i - minI, j - minJ) }
  }

  // I want area of polygon defined by given corners

  // val part1 = "a"
  // println(s"Part 1: $part1")

  // val part2 = "a"
  // println(s"Part 2: $part2")
}
