package year2023

import scala.collection.mutable
import scala.io.Source

def day17(): Unit = {
  val source = Source.fromFile("resources/2023/day-17")
  val heat = source.getLines.toVector
    .map { r => r.map(_.asDigit).toVector }
  source.close()

  val destination = (heat.size - 1, heat.size - 1)

  val part1 = pathWithMinimalHeatLoss(
    heat,
    { (rn, _) => rn < 3 },
    { state => state.pos == destination }
  )
  println(s"Part 1: $part1")

  val part2 = pathWithMinimalHeatLoss(
    heat,
    { (rn, rc) => (rn == 0 && rc >= 3) || (rn > 0 && rn <= 9) },
    { state => state.pos == destination && state.repeats >= 3 }
  )
  println(s"Part 2: $part2")
}

private def pathWithMinimalHeatLoss(
    heat: Vector[Vector[Int]],
    rule: (Int, Int) => Boolean,
    endRule: State => Boolean
) = {
  val distances = mutable.Map
    .empty[((Int, Int), (Int, Int), Int), Int]
    .withDefaultValue(Integer.MAX_VALUE)
  val directions = Vector((-1, 0), (1, 0), (0, -1), (0, 1))

  // val pathMap = mutable.Map.empty[State, State]
  val startState = State((0, 0), 0, (0, 0), 5)
  var endState: State = startState
  implicit val stateOrdering: Ordering[State] =
    Ordering.by[State, Int](_.distance).reverse
  val work = mutable.PriorityQueue[State](startState)
  while (work.nonEmpty) {
    val current = work.dequeue()
    val (r, c) = current.pos

    val newStates = directions
      .filterNot { (n, m) =>
        val (dr, dc) = current.direction
        dr == -n && dc == -m
      }
      .flatMap { (n, m) =>
        val repeats =
          if (current.direction == (n, m))
            current.repeats + 1
          else
            0

        val i = r + n
        val j = c + m
        if (i >= 0 && i < heat.size && j >= 0 && j < heat.size)
          Some(State((i, j), current.distance + heat(i)(j), (n, m), repeats))
        else
          None
      }
      .filter { s => rule(s.repeats, current.repeats) }
      .filter { s => s.distance < distances((s.pos, s.direction, s.repeats)) }

    newStates.find(endRule) match {
      case Some(state) =>
        endState = state
        work.clear()
      case None =>
        newStates.foreach { s =>
          distances((s.pos, s.direction, s.repeats)) = s.distance
          work.enqueue(s)
          // pathMap(s) = current
        }
    }
  }

  // val endState = pathMap.keys.filter(endRule).toVector.minBy(_.distance)
  // pprintStates(getPath(pathMap, endState), heat)

  endState.distance
}

private def getPath(pathMap: mutable.Map[State, State], endState: State) =
  LazyList
    .iterate(endState) { state => pathMap.getOrElse(state, endState) }
    .takeWhile(pathMap.contains)
    .toVector

sealed case class State(
    pos: (Int, Int),
    distance: Int,
    direction: (Int, Int),
    repeats: Int
)

private def pprintStates(
    states: Vector[State],
    heatMap: Vector[Vector[Int]]
): Unit = {
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
