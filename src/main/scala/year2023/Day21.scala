package year2023

import scala.io.Source
import scala.collection.mutable

def day21(): Unit = {
  val source = Source.fromFile("resources/2023/day-21")
  val gardenMap = source.getLines().toVector.map(_.toVector)
  source.close()

  val startPosition = gardenMap.zipWithIndex.flatMap { (v, i) =>
    v.zipWithIndex.collect { case ('S', j) => (i, j) }
  }.head
  val distances = findDistancesFromS(gardenMap, startPosition)

  val part1 = distances.values.filter(_ <= 64).count(_ % 2 == 0)
  println(s"Part 1: $part1")

  //  C1 b1
  //  O  B1 b1
  //  x  O  B1 .
  //  O  x  O  B1 b1
  //  x  O  x  O  B1 b1
  //  O  x  O  x  O  C2
  //
  //  plots =
  //    n^2 O
  //      + (n - 1)^2 X
  //      + C1 + ... + C4
  //      + n*(b1 + ... + b4)
  //      + (n-1)*(B1 + ... + B4)

  val maxSteps = 26501365L
  val n = (maxSteps - 65) / 131
  val dO = distances.values.count(_ % 2 == 0)
  val dX = distances.values.count(_ % 2 == 1)
  val dC1 = foo(gardenMap, (130, 65), 130)
  val dC2 = foo(gardenMap, (65, 0), 130)
  val dC3 = foo(gardenMap, (0, 65), 130)
  val dC4 = foo(gardenMap, (65, 130), 130)
  val db1 = foo(gardenMap, (130, 0), 64)
  val db2 = foo(gardenMap, (0, 0), 64)
  val db3 = foo(gardenMap, (130, 130), 64)
  val db4 = foo(gardenMap, (0, 130), 64)
  val dB1 = foo(gardenMap, (130, 1), 131 + 63)
  val dB2 = foo(gardenMap, (1, 0), 131 + 63)
  val dB3 = foo(gardenMap, (129, 130), 131 + 63)
  val dB4 = foo(gardenMap, (1, 130), 131 + 63)

  val part2 = (
    n*n*dO
    + (n - 1)*(n - 1)*dX
    + n*(db1 + db2 + db3 + db4)
    + (n - 1)*(dB1 + dB2 + dB3 + dB4)
    + dC1 + dC2 + dC3 + dC4
  )
  println(s"Part 2: $part2")
}

private def foo(
    gardenMap: Vector[Vector[Char]],
    startPosition: (Int, Int),
    length: Int,
) = {
  val distances = findDistancesFromS(gardenMap, startPosition)
  distances.values.filter(_ <= length).count(_ % 2 == 0)
}

private def findDistancesFromS(
    gardenMap: Vector[Vector[Char]],
    startPosition: (Int, Int)
) = {
  val distances = mutable
    .Map(startPosition -> 0)
    .withDefaultValue(Integer.MAX_VALUE)

  val size = gardenMap.size
  val directions = Vector((-1, 0), (1, 0), (0, -1), (0, 1))
  val work = mutable.PriorityQueue[((Int, Int), Int)]((startPosition, 0))
  while (work.nonEmpty) {
    val ((i0, j0), distance) = work.dequeue()

    val newStates: Unit = directions
      .foreach { (n, m) =>
        val i1 = i0 + n
        val j1 = j0 + m
        val newDistance = distance + 1
        if (
          (i1 >= 0 && i1 < size && j1 >= 0 && j1 < size)
          && gardenMap(i1)(j1) != '#'
          && newDistance < distances((i1, j1))
        )
          distances((i1, j1)) = newDistance
          work.enqueue(((i1, j1), newDistance))
      }
  }

  distances
}
