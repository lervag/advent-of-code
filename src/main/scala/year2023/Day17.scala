package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.PriorityQueue

def day17: Unit = {
  val source = Source.fromFile("resources/2023/day-17")
  val heatLoss = source.getLines.toVector
    .map { r => r.map(_.asDigit).toVector }
  source.close()

  val path = pathWithMinimalHeatLoss(
    heatLoss,
    (0, 0),
    (heatLoss.size - 1, heatLoss.size - 1)
  )

  // path.foreach(println)
  val part1 = path.head.distance
  println(s"Part 1: $part1")

  // val part2 = "asd"
  // println(s"Part 2: $part2")
}

private def pathWithMinimalHeatLoss(
  heat: Vector[Vector[Int]],
  start: (Int, Int),
  destination: (Int, Int)
) = {
  val startState = State(start, 0, (0, 0), 0)
  val work = PriorityQueue[State](startState)
  val distances = mutable.Map
    .empty[((Int, Int), (Int, Int), Int), Int]
    .withDefaultValue(Integer.MAX_VALUE)
  val directions = Vector((-1, 0), (1, 0), (0, -1), (0, 1))
  val pathMap = mutable.Map.empty[State, State]
  def getPath(endState: State) =
    LazyList
      .iterate(endState) { state => pathMap.getOrElse(state, endState) }
      .takeWhile(pathMap.contains(_))
      .toVector :+ startState

  while (!work.isEmpty) {
    val current = work.dequeue()
    val (r, c) = current.pos

    // println("\nNext:")
    // getPath(current).foreach(println)

    directions
      .filterNot { (n, m) =>
        val (dr, dc) = current.direction
        dr == -n && dc == -m
      }
      .flatMap { (n, m) =>
        val repeats = if (current.direction == (n, m))
          current.repeats + 1
        else
          0

        val i = r + n
        val j = c + m
        if (
          i >= 0 && i < heat.size && j >= 0 && j < heat.size
          // && repeats < 3
          && repeats < 3
        )
          Some(State((i, j), current.distance + heat(i)(j), (n, m), repeats))
        else
          None
      }
      .filter { s => s.distance < distances((s.pos, s.direction, s.repeats)) }
      .foreach { state =>
        distances((state.pos, state.direction, state.repeats)) = state.distance
        pathMap(state) = current
        work.enqueue(state)
      }
  }

  val endState = pathMap.keys.filter(_.pos == destination).minBy(_.distance)

  // pprintStates(getPath(endState), heat)


  getPath(endState)
}

implicit val stateOrdering: Ordering[State]
  = Ordering.by[State, Int](_.distance).reverse
case class State(pos: (Int, Int), distance: Int, direction: (Int, Int), repeats: Int)

private def pprintStates(states: Vector[State], heatMap: Vector[Vector[Int]]) = {
  val stateMap = states.groupBy(_.pos)
  heatMap.zipWithIndex.foreach { (row, i) =>
    val s = row.zipWithIndex.map { (cell, j) =>
      if (stateMap.contains((i, j)))
        stateMap((i, j)).head.direction match {
          case (1, 0) => ''
          case (-1, 0) => '^'
          case (0, -1) => '<'
          case (0, 1) => '>'
          case (0, 0) => 'x'
        }
      else cell
    }.mkString
    println(s)
  }
}
