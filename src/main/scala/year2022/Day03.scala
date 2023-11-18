package year2022

import scala.io.Source

def day03: Unit = {
  val source = Source.fromFile("resources/input-day-03")
  val lines = source.getLines.toVector
  source.close()
  val sum_priority = lines.map(getDuplicate).map(getPriority).sum
  val group_priority = lines
    .map(_.toSet)
    .grouped(3)
    .map { g => g.reduce(_ intersect _).head }
    .map(getPriority)
    .sum
  println(sum_priority)
  println(group_priority)
}

private def getDuplicate(s: String): Char = {
  val n = s.length()
  val s1 = s.substring(0, n / 2).toSet
  val s2 = s.substring(n / 2, n).toSet
  s1.intersect(s2).head
}

private def getPriority(c: Char) = {
  c.getNumericValue - 9 + (if c.isUpper then 26 else 0)
}
