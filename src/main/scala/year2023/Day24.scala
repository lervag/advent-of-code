package year2023

import scala.io.Source
import scala.collection.mutable

def day24(): Unit = {
  val source = Source.fromFile("resources/2023/day-24")
  val hails =
    source.getLines().toVector.map { case s"$x, $y, $z @ $u, $v, $w" =>
      Hail(
        x.toDouble,
        y.toDouble,
        z.toDouble,
        u.trim.toDouble,
        v.trim.toDouble,
        w.trim.toDouble
      )
    }
  source.close()

  val part1 = findIntersectionsInRegion(
    hails,
    200000000000000.0,
    400000000000000.0
  )
  println(s"Part 1: $part1")

  // val part2 = ???
  // println(s"Part 2: $part2")
}

sealed case class Hail(
    x: Double,
    y: Double,
    z: Double,
    u: Double,
    v: Double,
    w: Double
)

private def findIntersectionsInRegion(
    hails: Vector[Hail],
    min: Double,
    max: Double
) =
  hails
    .combinations(2)
    .flatMap { hh => intersection(hh(0), hh(1)) }
    .filter { (x, y) => x >= min && x <= max && y >= min && y <= max }
    .size

private def intersection(h1: Hail, h2: Hail): Option[(Double, Double)] = {
  val u1v1 = h1.u / h1.v
  val denom = h2.u - h2.v * u1v1

  if (denom == 0.0) None
  else
    val dx = h1.x - h2.x
    val dT = (dx - (h1.y - h2.y) * u1v1) / denom
    if (dT < 0.0) None
    else
      val dt = (dT * h2.u - dx) / h1.u
      if (dt < 0.0)
        None
      else
        Some((h1.x + dt * h1.u, h1.y + dt * h1.v))
}
