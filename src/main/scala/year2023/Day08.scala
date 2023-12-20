package year2023

import scala.io.Source

def day08: Unit = {
  val source = Source.fromFile("resources/2023/day-08")
  val lines = source.getLines.toVector
  source.close()

  val directions = lines(0).map {
    case 'L' => 0
    case 'R' => 1
  }.toVector
  val paths =
    lines.drop(2).map { case s"$x = ($y, $z)" => x -> Vector(y, z) }.toMap

  val part1 = Stream
    .iterate((0, "AAA")) { case (i, c) =>
      (i + 1, paths(c)(directions(i % directions.size)))
    }
    .takeWhile { case (i, _) => i < 100000 }
    .find { case (_, c) => c == "ZZZ" }
    .map { case (steps, _) => steps }
    .getOrElse(0)

  val getSteps: (BigInt, String) => BigInt = (startStep, startPos) => {
    Stream
      .iterate((startStep, startPos)) { case (i, c) =>
        (i + 1, paths(c)(directions((i % directions.size).toInt)))
      }
      .find { case (_, c) => c(2) == 'Z' }
      .map { case (n, _) => n }
      .getOrElse(0)
  }

  val part2 = paths.keys
    .filter { k => k(2) == 'A' }
    .toVector
    .map { n => getSteps(0, n) }
    .foldLeft(BigInt(1)) { (acc, factor) => lcm(acc, factor) }

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}

def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
def lcm(a: BigInt, b: BigInt) = (a * b) / gcd(a, b)
