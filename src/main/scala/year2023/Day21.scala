package year2023

import scala.io.Source
import scala.collection.mutable

def day21(): Unit = {
  val source = Source.fromFile("resources/2023/day-21")
  val gardenMap = source.getLines().toVector.map(_.toVector)
  source.close()

  var t0 = System.currentTimeMillis
  val startPosition = gardenMap.zipWithIndex.flatMap { (v, i) =>
    v.zipWithIndex.collect { case ('S', j) => (i, j) }
  }.head
  val part1 = findGardenSpotsFromS(gardenMap, startPosition, 64)
  println(s"Part 1: $part1")
  var t1 = System.currentTimeMillis
  println("Elapsed time: " + (t1 - t0) + "ms")

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
  val w = gardenMap.size
  val r = (w - 1) / 2
  val n = (maxSteps - r) / w
  t0 = System.currentTimeMillis
  val dO = findGardenSpotsFromS(gardenMap, (0, 0), 3*w)
  val dX = findGardenSpotsFromS(gardenMap, (0, 1), 3*w)
  val dC1 = findGardenSpotsFromS(gardenMap, (w - 1, r), w)
  val dC2 = findGardenSpotsFromS(gardenMap, (r, 0), w)
  val dC3 = findGardenSpotsFromS(gardenMap, (0, r), w)
  val dC4 = findGardenSpotsFromS(gardenMap, (r, w - 1), w)
  val db1 = findGardenSpotsFromS(gardenMap, (w - 1, 0), r)
  val db2 = findGardenSpotsFromS(gardenMap, (0, 0), r)
  val db3 = findGardenSpotsFromS(gardenMap, (w - 1, w - 1), r)
  val db4 = findGardenSpotsFromS(gardenMap, (0, w - 1), r)
  val dB1 = findGardenSpotsFromS(gardenMap, (w - 1, 1), w + r - 1)
  val dB2 = findGardenSpotsFromS(gardenMap, (1, 0), w + r - 1)
  val dB3 = findGardenSpotsFromS(gardenMap, (w - 2, w - 1), w + r - 1)
  val dB4 = findGardenSpotsFromS(gardenMap, (1, w - 1), w + r - 1)

  val part2 = (
    n * n * dO
      + (n - 1) * (n - 1) * dX
      + n * (db1 + db2 + db3 + db4)
      + (n - 1) * (dB1 + dB2 + dB3 + dB4)
      + dC1 + dC2 + dC3 + dC4
  )
  println(s"Part 2: $part2")
  t1 = System.currentTimeMillis
  println("Elapsed time: " + (t1 - t0) + "ms")

  // Create a polynomial
  //
  //   f(n) = a * n^2 + b * n + c
  //
  t0 = System.currentTimeMillis
  val f0 = findGardenSpotsFromSInfinite(gardenMap, startPosition, r)
  val f1 = findGardenSpotsFromSInfinite(gardenMap, startPosition, r + w)
  val f2 = findGardenSpotsFromSInfinite(gardenMap, startPosition, r + 2 * w)
  val c = f0
  val b = (4 * f1 - 3 * f0 - f2) / 2
  val a = f1 - b - c
  val ans = a * n * n + b * n + c
  println(s"Part 2: $ans")
  t1 = System.currentTimeMillis
  println("Elapsed time: " + (t1 - t0) + "ms")
}

private def findGardenSpotsFromS(
    gardenMap: Vector[Vector[Char]],
    startPosition: (Int, Int),
    maxSteps: Int
) = {
  val visited = mutable.Set(startPosition)
  var nSpots = 1
  val size = gardenMap.size
  val directions = Vector((-1, 0), (1, 0), (0, -1), (0, 1))
  val work = mutable.Queue[((Int, Int), Int)]((startPosition, 0))
  while (work.nonEmpty) {
    val ((i0, j0), previousSteps) = work.dequeue()

    val newStates: Unit = directions
      .foreach { (n, m) =>
        val i1 = i0 + n
        val j1 = j0 + m
        val nextSteps = previousSteps + 1
        if (
          (i1 >= 0 && i1 < size && j1 >= 0 && j1 < size)
          && nextSteps < maxSteps
          && gardenMap(i1)(j1) != '#'
          && !visited.contains((i1, j1))
        )
          visited += ((i1, j1))
          work.enqueue(((i1, j1), nextSteps))
          if (nextSteps % 2 == 0)
            nSpots += 1
      }
  }

  nSpots
}

private def findGardenSpotsFromSInfinite(
    gardenMap: Vector[Vector[Char]],
    startPosition: (Int, Int),
    maxSteps: Int
) = {
  val visited = mutable.Set(startPosition)
  var nSpots = 1
  val size = gardenMap.size
  val directions = Vector((-1, 0), (1, 0), (0, -1), (0, 1))
  val work = mutable.Queue[((Int, Int), Int)]((startPosition, 0))
  while (work.nonEmpty) {
    val ((i0, j0), previousSteps) = work.dequeue()

    val newStates: Unit = directions
      .foreach { (n, m) =>
        val i1 = i0 + n
        val j1 = j0 + m
        val ig = positiveModulus(i1, size)
        val jg = positiveModulus(j1, size)
        val nextSteps = previousSteps + 1
        if (
          nextSteps < maxSteps
          && gardenMap(ig)(jg) != '#'
          && !visited.contains((i1, j1))
        )
          visited += ((i1, j1))
          work.enqueue(((i1, j1), nextSteps))
          if (nextSteps % 2 == 0)
            nSpots += 1
      }
  }

  nSpots
}

private def positiveModulus(i: Int, modulus: Int): Int = {
  val result = i % modulus
  if (result < 0)
    result + modulus
  else
    result
}
