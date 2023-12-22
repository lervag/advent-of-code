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
  val width = gardenMap.size
  val m = maxSteps % width
  val n = (maxSteps - m) / width
  val hw = (width - 1) / 2
  val dO = distances.values.count(_ % 2 == 0)
  val dX = distances.values.count(_ % 2 == 1)
  val dC1 = getNGardenSpots(gardenMap, (width - 1, hw), width - 1)
  val dC2 = getNGardenSpots(gardenMap, (hw, 0), width - 1)
  val dC3 = getNGardenSpots(gardenMap, (0, hw), width - 1)
  val dC4 = getNGardenSpots(gardenMap, (hw, width - 1), width - 1)
  val db1 = getNGardenSpots(gardenMap, (width - 1, 0), hw - 1)
  val db2 = getNGardenSpots(gardenMap, (0, 0), hw - 1)
  val db3 = getNGardenSpots(gardenMap, (width - 1, width - 1), hw - 1)
  val db4 = getNGardenSpots(gardenMap, (0, width - 1), hw - 1)
  val dB1 = getNGardenSpots(gardenMap, (width - 1, 1), width + hw - 2)
  val dB2 = getNGardenSpots(gardenMap, (1, 0), width + hw - 2)
  val dB3 = getNGardenSpots(gardenMap, (129, width - 1), width + hw - 2)
  val dB4 = getNGardenSpots(gardenMap, (1, width - 1), width + hw - 2)

  val part2 = (
    n*n*dO
    + (n - 1)*(n - 1)*dX
    + n*(db1 + db2 + db3 + db4)
    + (n - 1)*(dB1 + dB2 + dB3 + dB4)
    + dC1 + dC2 + dC3 + dC4
  )

    // let grid_radius = (step_count / g.width) as usize;
    // let remainder = step_count % g.width;
    // let sample_garden_tiles_reached = walk_garden(&g, s, (g.width * 2 + remainder) as usize + 1);
    // // We want to create a polynomial f(n) = a * n^2 + b * n + c such that f(0) is the amount of tiles
    // // walked to at time `remainder`, f(1) is the amount of tiles walked to at time `remainder + grid.width`,
    // // f(2) is the amount of time walked to at `remainder + 2 * grid.width`
    // // We can verify f(0), f(1), f(2) against `sample_garden_tiles_reached`
    // let f_0 = sample_garden_tiles_reached[remainder as usize];
    // let f_1 = sample_garden_tiles_reached[(remainder + g.width) as usize];
    // let f_2 = sample_garden_tiles_reached[(remainder + 2 * g.width) as usize];
    // // Trivially, f(0) = c
    // let c = f_0;
    // // Now we have f_1 = a + b + c and f_2 = a * 4 + b * 2 + c
    // // Solve f_1 for a: a = f_1 - b - c, insert into f_2:
    // // f_2 = 4 * (f_1 - b - c) + b * 2 + c
    // // f_2 = 4 * f_1 - 2 * b - 3c => b = (4 * f_1 - 3c - f_2) / 2
    // let b = (4 * f_1 - 3 * c - f_2) / 2;
    // // f_1 = a + b + c => a = f_1 - b - c
    // let a = f_1 - b - c;
    // let ans = a * grid_radius * grid_radius + b * grid_radius + c; println(s"Part 2: $part2")
}

private def getNGardenSpots(
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
