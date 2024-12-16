package year2024

import cats.parse.{Parser, Rfc5234}

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.io.Source

def day16: Unit = {
  val choiceInput = List(
    ("resources/2024/day-16", (7036, 45)),
    ("resources/2024/day-16a", (11048, 64)),
    ("resources/2024/day-16b", (0, 0))
  )(2)

  val source = Source.fromFile(choiceInput._1)
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
    .map { case (w, s, e) => findShortestPath(s, 0, e, w) }
    .getOrElse((0, 0))

  println(part1)
  println(part2)
}

private def findShortestPath(
    startPos: Point,
    startDir: Int,
    target: Point,
    walls: Set[Point]
) = {
  case class State(path: Vector[Point], direction: Int, cost: Int)
  implicit val queueOrdering: Ordering[State] =
    Ordering.by[State, Int](_.cost).reverse
  val queue = PriorityQueue[State](State(Vector(startPos), startDir, 0))

  val visited = mutable.Map[(Point, Int), Int]()
  val bestTiles = mutable.Set[Point]()
  var finalCost = 0
  val directions = Vector((1, 0), (0, -1), (-1, 0), (0, 1))
  while (queue.nonEmpty) {
    val c = queue.dequeue()
    val position = c.path.last

    if position == target then
      finalCost = c.cost
      bestTiles ++= c.path
    else if !(
        walls.contains(position)
          || visited.get((position, c.direction)).exists(_ < c.cost)
          || (finalCost > 0 && c.cost > finalCost)
      )
    then
      visited((position, c.direction)) = c.cost
      val nextPos = position + directions(c.direction)
      queue += State(c.path :+ nextPos, c.direction, c.cost + 1)
      queue += State(c.path, (c.direction + 1) % 4, c.cost + 1000)
      queue += State(c.path, (c.direction + 3) % 4, c.cost + 1000)
  }

  (finalCost, bestTiles.size)
}
