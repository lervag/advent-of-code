package year2023

import scala.io.Source
import scala.collection.mutable

def day24(): Unit = {
  val source = Source.fromFile("resources/2023/day-24")
  val hails =
    source.getLines().toVector.map { case s"$x, $y, $z @ $u, $v, $w" =>
      Hail(
        x.toLong,
        y.toLong,
        z.toLong,
        u.trim.toLong,
        v.trim.toLong,
        w.trim.toLong
      )
    }
  source.close()

  // val part1 = findIntersectionsInRegion(
  //   hails,
  //   200000000000000.0,
  //   400000000000000.0
  // )
  // println(s"Part 1: $part1")

  val part2 = newtonSolver(hails(0), hails(1), hails(2))
  println(s"Part 2: $part2")
}

sealed case class Hail(
    x: Long,
    y: Long,
    z: Long,
    u: Long,
    v: Long,
    w: Long
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
  val u1v1 = h1.u.toDouble / h1.v
  val denom = h2.u - h2.v * u1v1

  if (denom == 0.0) None
  else
    val dx = h1.x - h2.x
    val t0 = (dx - (h1.y - h2.y) * u1v1) / denom
    if (t0 < 0.0) None
    else
      val t1 = (t0 * h2.u - dx) / h1.u
      if (t1 < 0.0)
        None
      else
        Some((h1.x + t1 * h1.u, h1.y + t1 * h1.v))
}

private def newtonSolver(h1: Hail, h2: Hail, h3: Hail) = {
  import breeze.linalg._
  // Ax = b
  val matrixA = DenseMatrix(
    ((h1.v - h2.v), (h2.u - h1.u),     0L    , (h2.y - h1.y), (h1.x - h2.x),     0L    ),
    ((h1.v - h3.v), (h3.u - h1.u),     0L    , (h3.y - h1.y), (h1.x - h3.x),     0L    ),
    ((h2.v - h3.v), (h3.u - h2.u),     0L    , (h3.y - h2.y), (h2.x - h3.x),     0L    ),
    ((h1.w - h2.w),     0L    , (h2.u - h1.u), (h2.z - h1.z),     0L    , (h1.x - h2.x)),
    ((h1.w - h3.w),     0L    , (h3.u - h1.u), (h3.z - h1.z),     0L    , (h1.x - h3.x)),
    ((h2.w - h3.w),     0L    , (h3.u - h2.u), (h3.z - h2.z),     0L    , (h2.x - h3.x))
  )

  println(matrixA)
  val x = matrixA.mapValues { v => v.toDouble }
  println(x)
  println(rank(x))

  h1
}

// Find stone (represented as a hail) H such that
//
//   t = collision(H, h) > 0 for all h in hails
//
// We need three known hails h. This gives 9 equations with 9 unknowns, that
// we solve with an iterative method.
//
// Written out, the idea is to solve:
//
//   (x - x1) + t1 * (u - u1) = 0
//   (y - y1) + t1 * (v - v1) = 0
//   (z - z1) + t1 * (w - w1) = 0
//   (x - x2) + t2 * (u - u2) = 0
//   (y - y2) + t2 * (v - v2) = 0
//   (z - z2) + t2 * (w - w2) = 0
//   (x - x3) + t3 * (u - u3) = 0
//   (y - y3) + t3 * (v - v3) = 0
//   (z - z3) + t3 * (w - w3) = 0
//
// Start by removing t1, t2 and t3:
//
//   (y - y1)(u - u1) - (x - x1)(v - v1) = 0
//   (z - z1)(u - u1) - (x - x1)(w - w1) = 0
//   (y - y2)(u - u2) - (x - x2)(v - v2) = 0
//   (z - z2)(u - u2) - (x - x2)(w - w2) = 0
//   (y - y3)(u - u3) - (x - x3)(v - v3) = 0
//   (z - z3)(u - u3) - (x - x3)(w - w3) = 0
//
// Write out and reorder the equations:
//
//   y u - y1 u - u1 y + u1 y1 - x v - x1 v1 + x1 v + v1 x = 0
//   y u - y2 u - u2 y + u2 y2 - x v - x2 v2 + x2 v + v2 x = 0
//   y u - y3 u - u3 y + u3 y3 - x v - x3 v3 + x3 v + v3 x = 0
//   z u - z1 u - u1 z + u1 z1 - x w - x1 w1 + x1 w + w1 x = 0
//   z u - z2 u - u2 z + u2 z2 - x w - x2 w2 + x2 w + w2 x = 0
//   z u - z3 u - u3 z + u3 z3 - x w - x3 w3 + x3 w + w3 x = 0
//
// Subtract equations to make them linear:
//
//   (v1 - v2) x + (u2 - u1) y + (y2 - y1) u + (x1 - x2) v = x1 v1 - x2 v2 + u2 y2 - u1 y1
//   (v1 - v3) x + (u3 - u1) y + (y3 - y1) u + (x1 - x3) v = x1 v1 - x3 v3 + u3 y3 - u1 y1
//   (v2 - v3) x + (u3 - u2) y + (y3 - y2) u + (x2 - x3) v = x2 v2 - x3 v3 + u3 y3 - u2 y2
//   (w1 - w2) x + (u2 - u1) z + (z2 - z1) u + (x1 - x2) w = x1 w1 - x2 w2 + u2 z2 - u1 z1
//   (w1 - w3) x + (u3 - u1) z + (z3 - z1) u + (x1 - x3) w = x1 w1 - x3 w3 + u3 z3 - u1 z1
//   (w2 - w3) x + (u3 - u2) z + (z3 - z2) u + (x2 - x3) w = x2 w2 - x3 w3 + u3 z3 - u2 z2
//
// These are now 6 linear equations with 6 variables x, y, z, u, v, w. Thus:
//
// [ (v1 - v2) (u2 - u1)     0     (y2 - y1) (x1 - x2)     0     ] [x]   [ x1 v1 - x2 v2 + u2 y2 - u1 y1 ]
// | (v1 - v3) (u3 - u1)     0     (y3 - y1) (x1 - x3)     0     | |y|   | x1 v1 - x3 v3 + u3 y3 - u1 y1 |
// | (v2 - v3) (u3 - u2)     0     (y3 - y2) (x2 - x3)     0     | |z|   | x2 v2 - x3 v3 + u3 y3 - u2 y2 |
// | (w1 - w2)     0     (u2 - u1) (z2 - z1)     0     (x1 - x2) | |u| = | x1 w1 - x2 w2 + u2 z2 - u1 z1 |
// | (w1 - w3)     0     (u3 - u1) (z3 - z1)     0     (x1 - x3) | |v|   | x1 w1 - x2 w2 + u2 z2 - u1 z1 |
// [ (w2 - w3)     0     (u3 - u2) (z3 - z2)     0     (x2 - x3) ] [w]   [ x2 w2 - x3 w3 + u3 z3 - u2 z2 ]
//
