package year2023

import scala.io.Source
import scala.annotation.tailrec

def day09: Unit = {
  val source = Source.fromFile("resources/2023/day-09")
  val lines = source.getLines.toVector
  source.close()

  val h = lines.map { line => line.split(" +").map(_.toInt).toVector }
  val part1 = h.map(extrapolateForward(_)).sum
  println(s"Part 1: $part1")

  val part2 = h.map(extrapolateBackward(_)).sum
  println(s"Part 2: $part2")
}

private def extrapolateBackward(h: Vector[Int]): Int =
  extrapolateBackwardRecurse(h.head, true, diff(h))

@tailrec
private def extrapolateBackwardRecurse(n: Int, sign: Boolean, d: Vector[Int]): Int =
  if (d.exists(_ != 0))
    val newN = if (sign) then -d.head else d.head
    extrapolateBackwardRecurse(n + newN, !sign, diff(d))
  else
    n

private def extrapolateForward(h: Vector[Int]): Int =
  extrapolateForwardRecurse(h.last, diff(h))

@tailrec
private def extrapolateForwardRecurse(n: Int, d: Vector[Int]): Int =
  if (d.exists(_ != 0))
    extrapolateForwardRecurse(n + d.last, diff(d))
  else
    n

private def diff(v: Vector[Int]): Vector[Int] =
  v.sliding(2).map { a => a(1) - a(0) }.toVector
