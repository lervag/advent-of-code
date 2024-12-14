package year2023

import scala.io.Source
import scala.collection.mutable
import scala.annotation.tailrec

def day23(): Unit = {
  val source = Source.fromFile("resources/2023/day-23")
  val trails = source.getLines().toVector.map(_.toVector)
  source.close()

  val part1 = findLongestHikes(trails)
  println(s"Part 1: $part1")

  val part2 = findLongestHikesNS(trails)
  println(s"Part 2: $part2")
}

private def findLongestHikesNS(trails: Vector[Vector[Char]]) = {
  val startPosition = Position(0, trails(0).indexOf('.'))
  val targetPosition =
    Position(trails.size - 1, trails(trails.size - 1).indexOf('.'))

  val graph = trailToGraph(trails)
  // val hikes = mutable.Map.empty[Int, Vector[Position]]
  var maxDistance = 0

  val startState = StateHikeNS(startPosition, 0)
  implicit val stateOrdering: Ordering[StateHikeNS] = Ordering.by {
    (s: StateHikeNS) => s.distance
  }
  val work = mutable.PriorityQueue[StateHikeNS](startState)
  while (work.nonEmpty) {
    val current = work.dequeue()

    graph(current.p)
      .filter { (nextPos, _) => !current.visited.contains(nextPos) }
      .foreach { (nextPos, d) =>
        val nextState = StateHikeNS(
          nextPos,
          distance = current.distance + d,
          visited = current.visited :+ current.p
        )

        if (nextPos == targetPosition)
          // hikes(nextState.distance) = nextState.visited :+ nextPos
          maxDistance = maxDistance.max(nextState.distance)
        else
          work.enqueue(nextState)
      }
  }

  maxDistance
}

private def trailToGraph(trails: Vector[Vector[Char]]) = {
  val n = trails.size
  val startPosition = Position(0, trails(0).indexOf('.'))
  val startPositionNext = startPosition.copy(i = 1)
  val targetPosition = Position(n - 1, trails(n - 1).indexOf('.'))

  val graph = mutable.Map(startPosition -> mutable.Map.empty[Position, Int])

  val directions = Direction.values.toVector
  @tailrec
  def findNextNode(
      prevPos: Position,
      curPos: Position,
      distance: Int = 1
  ): (Position, Vector[Position], Int) = {
    val nextPositions = directions
      .map { d => curPos.go(d) }
      .filter { p =>
        p.i >= 0 && p.i < n && trails(p.i)(p.j) != '#' && p != prevPos
      }

    if (nextPositions.size > 1) (curPos, nextPositions, distance)
    else
      val next = nextPositions.head
      if (next == targetPosition)
        (next, Vector.empty[Position], distance + 1)
      else
        findNextNode(curPos, next, distance + 1)
  }

  val queue = mutable.Queue((startPosition, startPositionNext))
  while (queue.nonEmpty) {
    val (prevNodePos, currentPos) = queue.dequeue()
    val (nextNodePos, nextPositions, distance) =
      findNextNode(prevNodePos, currentPos)

    graph(prevNodePos)(nextNodePos) = distance

    if (graph.contains(nextNodePos)) graph(nextNodePos)(prevNodePos) = distance
    else
      graph(nextNodePos) = mutable.Map(prevNodePos -> distance)
      nextPositions.foreach { newPos => queue.enqueue((nextNodePos, newPos)) }
  }

  graph.view.mapValues(_.toVector).toMap
}

private def findLongestHikes(
    trails: Vector[Vector[Char]]
) = {
  val n = trails.size
  val startPosition = Position(0, trails(0).indexOf('.'))
  val targetPosition = Position(n - 1, trails(n - 1).indexOf('.'))
  val startState = StateHike(startPosition, Direction.Down, false, 0)

  implicit val stateOrdering: Ordering[StateHike] = Ordering.by {
    (s: StateHike) => s.distance
  }
  val work = mutable.PriorityQueue[StateHike](startState)

  val hikes = mutable.Map.empty[Int, Vector[Position]]
  val directions = Direction.values.toVector
  while (work.nonEmpty) {
    val current = work.dequeue()

    val nextPositions =
      if (current.force)
        Vector(current.p.go(current.d))
          .filter { p => !current.visited.contains(p) }
      else
        directions
          .map { d => current.p.go(d) }
          .filter { p =>
            (
              p.i >= 0 && p.i < n && p.j >= 0 && p.j < n
              && trails(p.i)(p.j) != '#'
              && !current.visited.contains(p)
            )
          }

    nextPositions
      .foreach { nextPos =>
        val (nextDir, forced) = trails(nextPos.i)(nextPos.j) match {
          case '<' => (Direction.Left, true)
          case '>' => (Direction.Right, true)
          case '^' => (Direction.Up, true)
          case 'v' => (Direction.Down, true)
          case _ => (current.d, false)
        }
        val nextState = StateHike(
          nextPos,
          nextDir,
          forced,
          distance = current.distance + 1,
          visited = current.visited :+ current.p
        )

        if (nextPos == targetPosition)
          hikes(nextState.distance) = nextState.visited :+ nextPos
        else
          work.enqueue(nextState)
      }
  }

  hikes.keys.max
}

private def pprint(
    trails: Vector[Vector[Char]],
    distance: Int,
    positions: Vector[Position]
): Unit = {
  val n = trails.size
  (0 until n) foreach { i =>
    val line = (0 until n) map { j =>
      if (positions.contains(Position(i, j)))
        if (trails(i)(j) == '#')
          'X'
        else
          'O'
      else
        trails(i)(j)
    }
    println(line.mkString)
  }
  println(s"Total distance: $distance\n")
}

case class Position(i: Int, j: Int) {
  def go(d: Direction): Position = d match {
    case Direction.Left => Position(i, j - 1)
    case Direction.Right => Position(i, j + 1)
    case Direction.Up => Position(i - 1, j)
    case Direction.Down => Position(i + 1, j)
  }
}

sealed case class StateHikeNS(
    p: Position,
    distance: Int,
    visited: Vector[Position] = Vector.empty[Position]
)

sealed case class StateHike(
    p: Position,
    d: Direction,
    force: Boolean,
    distance: Int,
    visited: Vector[Position] = Vector.empty[Position]
)
