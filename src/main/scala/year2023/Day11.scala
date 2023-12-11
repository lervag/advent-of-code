package year2023

import scala.io.Source

def day11: Unit = {
  val source = Source.fromFile("resources/2023/day-11")
  val lines = source.getLines.toVector
  source.close()

  val galaxies = lines.zipWithIndex.flatMap { (line, i) =>
    line.zipWithIndex.collect { case ('#', j) => (i, j) }
  }
  val emptyRows = (0 until lines.size).toSet -- galaxies.map(_._1).toSet
  val emptyCols = (0 until lines.size).toSet -- galaxies.map(_._2).toSet

  def expandAndSum(n: Int): BigInt =
    galaxies
      .map { (i, j) =>
        (i + n * emptyRows.count(_ < i), j + n * emptyCols.count(_ < j))
      }
      .combinations(2)
      .map { points => distance(points(0), points(1)) }
      .sum

  val part1 = expandAndSum(1)
  println(s"Part 1: $part1")

  val part2 = expandAndSum(999999)
  println(s"Part 2: $part2")
}

private def distance(p1: (Int, Int), p2: (Int, Int)): BigInt = {
  val a = (p2._1 - p1._1).abs
  val b = (p2._2 - p1._2).abs
  BigInt(a) + BigInt(b)
}
