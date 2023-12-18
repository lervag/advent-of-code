package year2023

import scala.io.Source

def day18: Unit = {
  val source = Source.fromFile("resources/2023/day-18")
  val spec = source.getLines.toVector
    .map { case s"$direction $length (#$rgb)" =>
      (direction, length.toInt, rgb)
    }
  source.close()

  val corners1 = parseCorners(spec)
  val part1 = area(corners1)
  println(s"Part 1: $part1")

  val corners2 = parseCorners(spec.map { (_, _, rgb) =>
    val (dir, length) = parseColor(rgb)
    (dir, length, rgb)
  })
  val part2 = area(corners2)
  println(s"Part 2: $part2")
}

private def parseCorners(spec: Vector[(String, Int, String)]) = {
  val cornersAux = spec
    .scanLeft((0, 0)) { case ((i, j), (dir, length, _)) =>
      dir match {
        case "U" => (i - length, j)
        case "D" => (i + length, j)
        case "L" => (i, j - length)
        case "R" => (i, j + length)
      }
    }
  val minI = cornersAux.map(_._1).min
  val minJ = cornersAux.map(_._2).min
  cornersAux
    .map { (i, j) => (i - minI, j - minJ) }
    .map { (i, j) => (BigInt(i), BigInt(j)) }
    .reverse
}

private def parseColor(c: String) = {
  val length = Integer.parseInt(c.take(5), 16)
  val dir = c(5) match {
    case '0' => "R"
    case '1' => "D"
    case '2' => "L"
    case '3' => "U"
  }
  (dir, length)
}

private def area(corners: Vector[(BigInt, BigInt)]) = {
  val area = areaShoelace(corners)
  val b = circumference(corners)
  innerPointsFromPicksTheorem(area, b) + b
}

private def circumference(points: Vector[(BigInt, BigInt)]) =
  points
    .zip(points.tail)
    .map { case ((i0, j0), (i1, j1)) => (i0 - i1).abs + (j0 - j1).abs }
    .sum

private def areaShoelace(points: Vector[(BigInt, BigInt)]) =
  points
    .zip(points.tail)
    .map { case ((i0, j0), (i1, j1)) => i0 * j1 - i1 * j0 }
    .sum / 2

private def innerPointsFromPicksTheorem(area: BigInt, b: BigInt) =
  area - b / 2 + 1
