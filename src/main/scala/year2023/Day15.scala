package year2023

import scala.io.Source
import scala.collection.mutable.LinkedHashMap

def day15: Unit = {
  val source = Source.fromFile("resources/2023/day-15")
  val sequence = source.getLines().next().split(",").toVector
  source.close()

  val part1 = sequence.map(hash).sum
  println(s"Part 1: $part1")

  val part2 = sequence
    .flatMap {
      case s"$label=$n" => Some((hash(label), label, n.toInt))
      case s"$label-"   => Some((hash(label), label, 0))
      case _            => None
    }
    .groupBy(_._1)
    .mapValues(_.foldLeft(LinkedHashMap.empty[String, Int]) {
      case (acc, (_, label, n)) =>
        if (n == 0) acc - label
        else acc + (label -> n)
    })
    .map { (hashed, lenses) =>
      lenses.zipWithIndex.map { case ((_, lens), index) =>
        (hashed + 1) * (index + 1) * lens
      }.sum
    }
    .sum
  println(s"Part 2: $part2")
}

private def hash(s: String): Int =
  s.foldLeft(0) { (v, c) => 17 * (v + c.toInt) % 256 }
