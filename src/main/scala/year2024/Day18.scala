package year2024

import cats.parse.{Numbers, Parser, Rfc5234}

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.io.Source

def day18: Unit = {
  val (choiceInput, size, takeBytes) = List(
    ("resources/2024/day-18", 6, 12),
    ("resources/2024/day-18a", 70, 1024)
  )(1)

  val source = Source.fromFile(choiceInput)
  val input = source.getLines().mkString("\n")
  source.close()

  val p_num = Numbers.digits.map(_.toInt)
  val parser = (p_num ~ Parser.char(',') ~ p_num)
    .map { case (((x, _), y)) => (x, y) }
    .repSep0(Rfc5234.lf)

  val start = (0, 0)
  val target = (size, size)

  val part1 = parser
    .parseAll(input)
    .map { b => findShortestPath(start, target, b.take(takeBytes).toSet) }
    .getOrElse(0)

  val part2 = parser
    .parseAll(input)
    .map { b =>
      bsForTrue(
        0,
        b.size,
        { k => findShortestPath(start, target, b.take(k).toSet) == 0 }
      )
    }
    .getOrElse(0)

  println(part1)
  println(part2)
}

private def findShortestPath(
    startPos: Point,
    target: Point,
    corrupted: Set[Point]
) = {
  case class State(pos: Point, steps: Int)
  implicit val queueOrdering: Ordering[State] =
    Ordering.by[State, Int](_.steps).reverse
  val queue = PriorityQueue[State](State(startPos, 0))
  val visited = mutable.Set[Point](startPos)

  val size = target._1 + 1
  var steps = 0
  val directions = Vector((1, 0), (0, -1), (-1, 0), (0, 1))
  while (queue.nonEmpty) {
    val c = queue.dequeue()

    directions.foreach { dir =>
      val next = c.pos + dir
      if (
        next.isInGrid(size, size)
        && !corrupted.contains(next)
        && !visited.contains(next)
      )
      then
        visited += next
        queue += State(next, c.steps + 1)
    }

    if c.pos == target then
      steps = c.steps
      queue.clear()
  }

  steps
}

private def bsForTrue(low: Int, high: Int, f: Int => Boolean): Int = {
  var left = low
  var right = high

  while (left < right) {
    val mid = left + (right - left) / 2
    if f(mid)
    then right = mid
    else left = mid + 1
  }

  if (right < high) then right
  else 0
}
