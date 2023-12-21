package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

def day21: Unit = {
  val source = Source.fromFile("resources/2023/day-21")
  val gardenMap = source.getLines().toVector.map(_.toVector)
  source.close()

  val distances = findDistancesFromS(gardenMap)
  val part1 = distances.values.filter(_ <= 64).filter(_ % 2 == 0).size
  println(s"Part 1: $part1")

  val maxSteps = 26501365
  // gardenMap repeats infinitely

  // val part2 =
  // println(s"Part 2: $part2")
}

private def findDistancesFromS(
  gardenMap: Vector[Vector[Char]],
) = {
  val startPosition = gardenMap.zipWithIndex
    .flatMap { (v, i) =>
      v.zipWithIndex.collect { case ('S', j) => (i, j) }
    }
    .head

  val distances = mutable.Map(startPosition -> 0)
    .withDefaultValue(Integer.MAX_VALUE)

  val size = gardenMap.size
  val directions = Vector((-1, 0), (1, 0), (0, -1), (0, 1))
  val work = PriorityQueue[((Int, Int), Int)]((startPosition, 0))
  while (!work.isEmpty) {
    val ((i0, j0), distance) = work.dequeue()

    val newStates = directions
      .foreach { (n, m) =>
        val i1 = i0 + n
        val j1 = j0 + m
        val newDistance = distance + 1
        if (
          (i1 >= 0 && i1 < size && j1 >= 0 && j1 < size)
          && gardenMap(i1)(j1) == '.'
          && newDistance < 65
          && newDistance < distances((i1, j1))
        )
          distances((i1, j1)) = newDistance
          work.enqueue(((i1, j1), newDistance))
      }
  }

  distances
}
