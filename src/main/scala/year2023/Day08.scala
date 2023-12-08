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
  val paths = lines.drop(2).map { case s"$x = ($y, $z)" => x -> Vector(y, z) }.toMap

  Stream.iterate((0, "AAA")) { (i, c) =>
    (i + 1, paths(c)(directions(i % directions.size)))
  }
    .takeWhile { (i, _) => i < 100000 }
    .find { (_, c) => c == "ZZZ" }
    match {
      case Some((steps, _)) => println(s"Part 1: $steps")
      case None    => None
    }

  def getSteps(startStep: BigInt, startPos: String): BigInt =
    Stream.iterate((startStep, startPos)) { (i, c) =>
      (i + 1, paths(c)(directions((i % directions.size).toInt)))
    }
      .find { (_, c) => c(2) == 'Z' }
      match {
        case Some(n, _) => n
        case None    => 0
      }
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  val firstZ = paths.keys
    .filter { k => k(2) == 'A' }.toVector
    .map { n => getSteps(0, n) }
  val lcm = firstZ.foldLeft(BigInt(1)) { (acc, factor) =>
    (acc * factor)/gcd(acc, factor)
  }
  println(s"Part 2: $lcm")
}

