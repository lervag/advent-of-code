package year2024

import scala.io.Source

def day08: Unit = {
  val source = Source.fromFile("resources/2024/day-08a")
  val lines = source.getLines().toVector
  source.close()

  val mapSize = lines.length
  val antennas: Map[Char, Vector[Position]] = lines.zipWithIndex
    .flatMap { (line, j) =>
      line.zipWithIndex.collect {
        case (c, i) if c != '.' => (c, Position(i, j))
      }
    }
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2))
    .toMap
    .filter { (_, ps) => ps.size > 1 }

  def isInside(p: Position): Boolean =
    p.i >= 0 && p.i < mapSize && p.j >= 0 && p.j < mapSize

  def countAntinodes(
      combiner: ((Position, Position)) => Vector[Position]
  ): Int =
    antennas
      .flatMap { (_, positions) =>
        positions
          .combinations(2)
          .collect { case Vector(a, b) =>
            (a, b)
          }
          .toVector
          .flatMap(combiner)
      }
      .toSet
      .size

  val part1 = countAntinodes { (a, b) =>
    Vector(
      Position(2 * a.i - b.i, 2 * a.j - b.j),
      Position(2 * b.i - a.i, 2 * b.j - a.j)
    )
      .filter(isInside)
  }
  println(part1)

  val part2 = countAntinodes { (a, b) =>
    val dx = b.i - a.i
    val dy = b.j - a.j
    Vector((a, -1), (b, 1)).flatMap { (s, d) =>
      (0 to mapSize).iterator
        .map(k => Position(s.i + k * d * dx, s.j + k * d * dy))
        .takeWhile(isInside)
    }
  }
  println(part2)
}
