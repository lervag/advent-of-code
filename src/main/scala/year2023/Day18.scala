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
    cornersAux
      .map { (i, j) => (i - minI, j - minJ) }
      .sliding(2)
      .toVector
      .map(_ match {
        case Vector((i0, j0), (i1, j1)) if i0 == i1 =>
          if (j0 < j1)
            (j0 until j1).map((i0, _))
          else
            (j0 until j1 by -1).map((i0, _))
        case Vector((i0, j0), (i1, j1)) if j0 == j1 =>
          if (i0 < i1)
            (i0 until i1).map((_, j0))
          else
            (i0 until i1 by -1).map((_, j0))
        case _ => Vector((0, 0))
      })
      .flatten
  }

  val maxI = corners.map(_._1).max
  val maxJ = corners.map(_._2).max
  // val digarea = (0 to maxI).toVector.map { i =>
  //   (0 to maxJ).toVector.map { j =>
  //     if (corners.contains((i, j)))
  //       '#'
  //     else
  //       '.'
  //   }
  // }
  // digarea.foreach { row => println(row.mkString) }
  val part1 = (0 to maxI).toVector.map { i =>
    var inside = false
    var prevEdge = false
    (0 to maxJ).foldLeft(0) { (acc, j) =>
      if (corners.contains((i, j)) && !prevEdge)
        inside = !inside

      prevEdge = corners.contains((i, j))
      if (inside || corners.contains((i, j)))
        acc + 1
      else
        acc
    }
  }.sum

  // val part1 = "a"
  println(s"Part 1: $part1")

  // val part2 = "a"
  // println(s"Part 2: $part2")
}
