package year2023

import scala.io.Source
import scala.collection.mutable

def day17: Unit = {
  val source = Source.fromFile("resources/2023/day-17a")
  val heatLoss = source.getLines.toVector
    .map { r => r.map(Integer(_)).toVector }
  source.close()

  val s = (0, 0)
  val e = (heatLoss.size, heatLoss.size)
  // move at most three blocks in a straight line
  // cant reverse
  val part1 = "asd"
  println(s"Part 1: $part1")

  val part2 = "asd"
  println(s"Part 2: $part2")
}
