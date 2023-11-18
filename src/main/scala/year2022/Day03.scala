package year2022

import ujson._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.{Map => MutableMap}
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Using

def day03: Unit = {
  val source = Source.fromFile("resources/input-day-03")
  val lines = source.getLines.toList
  // toVector er bedre
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
