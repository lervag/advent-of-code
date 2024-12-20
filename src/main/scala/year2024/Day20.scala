package year2024

import cats.parse.{Parser, Rfc5234}

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

def day20: Unit = {
  val (choiceInput, cutOff) = List(
    ("resources/2024/day-20", 50),
    ("resources/2024/day-20a", 100)
  )(1)

  val source = Source.fromFile(choiceInput)
  val input = source.getLines().mkString("\n")
  source.close()

  val parser = (Parser.charIn('#', '.', 'E', 'S').rep <* Rfc5234.lf.rep0).rep
    .map { lists =>
      val coordinates = lists.toList.zipWithIndex
        .flatMap { (row, j) =>
          row.zipWithIndex.collect {
            case (char, i) if char != '.' => (char, (i, j))
          }
        }
        .groupMap(_._1) { case (_, point: Point) =>
          point
        }
      val walls = coordinates('#').toSet
      val startpos = coordinates('S').head
      val endpos = coordinates('E').head
      (walls, startpos, endpos)
    }

  val (part1, part2) = parser
    .parseAll(input)
    .map { case (w, s, e) =>
      val distanceMap = findDistanceMap(s, e, w)
      val distances = findCheatPaths(s, e, w, distanceMap)
      val p1 = distances.values.filter(_ >= cutOff).size

      val path = distanceMap.keys.toVector
      val p2 = distanceMap.toVector.flatMap { (p, d) =>
        findCheats(p, d, path, distanceMap, 20, cutOff)
      }.size

      (p1, p2)
    }
    .getOrElse((0, 0))

  println(part1)
  println(part2)
}

private def findCheats(
    cheatStart: Point,
    startDistance: Int,
    path: Vector[Point],
    distanceMap: Map[Point, Int],
    cheatLength: Int,
    cutOff: Int
) =
  path
    .map { cheatEnd =>
      val moves = cheatStart.distanceTo(cheatEnd)
      (cheatEnd, moves)
    }
    .filter { (_, m) => m > 0 && m <= cheatLength }
    .map { (cheatEnd, moves) =>
      startDistance - distanceMap(cheatEnd) - moves
    }
    .filter { s => s >= cutOff }

private def findDistanceMap(start: Point, target: Point, walls: Set[Point]) = {
  case class State(position: Point, moves: Int = 0)
  given stateOrdering: Ordering[State] =
    Ordering.by[State, Int](_.moves).reverse

  val size = walls.map(_._1).max
  val directions = Vector((1, 0), (0, -1), (-1, 0), (0, 1))
  val queue = PriorityQueue[State](State(start))
  val distances = mutable
    .Map[Point, Int]((start, 0))
    .withDefaultValue(Int.MaxValue)

  var distance = 0
  while (queue.nonEmpty) {
    val c = queue.dequeue()

    if c.position == target then
      distance = c.moves
      queue.clear()
    else
      directions
        .map { dir => c.position + dir }
        .filter { newPos =>
          newPos.isInGrid(size, size)
          && !walls.contains(newPos)
          && distances(newPos) > c.moves
        }
        .foreach { newPos =>
          distances(newPos) = c.moves + 1
          queue += State(newPos, c.moves + 1)
        }
  }

  distances.view
    .mapValues(distance - _)
    .toMap
}

private def findCheatPaths(
    start: Point,
    target: Point,
    walls: Set[Point],
    distances: Map[Point, Int]
) = {
  case class State(position: Point, moves: Int = 0)
  given stateOrdering: Ordering[State] =
    Ordering.by[State, Int](_.moves).reverse

  val maxMoves = distances.values.max
  val size = walls.map(_._1).max
  val directions = Vector((1, 0), (0, -1), (-1, 0), (0, 1))
  val queue = PriorityQueue[State](State(start))
  val visited = mutable.Set[Point](start)

  val cheatSaves = mutable.Map[(Point, Point), Int]()

  while (queue.nonEmpty) {
    val c = queue.dequeue()

    if c.moves >= maxMoves then queue.clear()
    else
      directions
        .map { dir => (c.position + dir, dir) }
        .filter { (p, _) => p.isInGrid(size, size) && !visited.contains(p) }
        .foreach { (newPos, dir) =>
          if walls.contains(newPos) then
            val cheatPos = newPos + dir
            if cheatPos.isInGrid(size, size)
              && !walls.contains(cheatPos)
              && distances(cheatPos) < distances(c.position)
            then
              cheatSaves((c.position, cheatPos)) =
                (maxMoves - c.moves - 2) - distances(cheatPos)
          else
            visited += newPos
            queue += State(newPos, c.moves + 1)
        }

  }

  cheatSaves
}
