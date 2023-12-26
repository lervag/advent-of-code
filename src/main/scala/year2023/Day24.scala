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

  val part1 = findIntersectionsInRegion(
    hails,
    200000000000000.0,
    400000000000000.0
  )
  println(s"Part 1: $part1")

  val part2 = newtonSolver(hails(0), hails(1), hails(2), hails(3))
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

private def newtonSolver(h1: Hail, h2: Hail, h3: Hail, h4: Hail) = {
  import breeze.linalg._
  val matrixA = DenseMatrix(
    ((h2.v - h1.v), (h1.u - h2.u),     0L    , (h1.y - h2.y), (h2.x - h1.x),     0L    ),
    ((h3.v - h1.v), (h1.u - h3.u),     0L    , (h1.y - h3.y), (h3.x - h1.x),     0L    ),
    ((h4.v - h1.v), (h1.u - h4.u),     0L    , (h1.y - h4.y), (h4.x - h1.x),     0L    ),
    ((h2.w - h1.w),     0L    , (h1.u - h2.u), (h1.z - h2.z),     0L    , (h2.x - h1.x)),
    ((h3.w - h1.w),     0L    , (h1.u - h3.u), (h1.z - h3.z),     0L    , (h3.x - h1.x)),
    ((h4.w - h1.w),     0L    , (h1.u - h4.u), (h1.z - h4.z),     0L    , (h4.x - h1.x)),
  )

  val b = DenseVector(
    h1.u*h1.y - h1.x*h1.v - h2.u*h2.y + h2.x*h2.v,
    h1.u*h1.y - h1.x*h1.v - h3.u*h3.y + h3.x*h3.v,
    h1.u*h1.y - h1.x*h1.v - h4.u*h4.y + h4.x*h4.v,
    h1.u*h1.z - h1.x*h1.w - h2.u*h2.z + h2.x*h2.w,
    h1.u*h1.z - h1.x*h1.w - h3.u*h3.z + h3.x*h3.w,
    h1.u*h1.z - h1.x*h1.w - h4.u*h4.z + h4.x*h4.w
  )

  val x = inv(matrixA) * b.mapValues(_.toDouble)
  x(0).toLong + x(1).toLong + x(2).toLong + 1
}

// Find stone (represented as a hail) H such that
//
//   t = collision(H, h) > 0 for all h in hails
//
// We need three known hails h. This gives 9 equations with 9 unknowns.
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
//   (y - y2)(u - u2) - (x - x2)(v - v2) = 0
//   (y - y3)(u - u3) - (x - x3)(v - v3) = 0
//   (z - z1)(u - u1) - (x - x1)(w - w1) = 0
//   (z - z2)(u - u2) - (x - x2)(w - w2) = 0
//   (z - z3)(u - u3) - (x - x3)(w - w3) = 0
//
// Or:
//
//   v1 x - u1 y - y1 u + x1 v + u1 y1 - x1 v1 = x v - y u
//   v2 x - u2 y - y2 u + x2 v + u2 y2 - x2 v2 = x v - y u
//   v3 x - u3 y - y3 u + x3 v + u3 y3 - x3 v3 = x v - y u
//   w1 x - u1 z - z1 u + x1 w + u1 z1 - x1 w1 = x w - z u
//   w2 x - u2 z - z2 u + x2 w + u2 z2 - x2 w2 = x w - z u
//   w3 x - u3 z - z3 u + x3 w + u3 z3 - x3 w3 = x w - z u
//
// The right-hand side has nonlinear terms. We can use that and add a hail to
// get 6 linear equations:
//
//   (v2 - v1) x + (u1 - u2) y + (y1 - y2) u + (x2 - x1) v = u1 y1 - x1 v1 - u2 y2 + x2 v2
//   (v3 - v1) x + (u1 - u3) y + (y1 - y3) u + (x3 - x1) v = u1 y1 - x1 v1 - u3 y3 + x3 v3
//   (v4 - v1) x + (u1 - u4) y + (y1 - y4) u + (x4 - x1) v = u1 y1 - x1 v1 - u4 y4 + x4 v4
//   (w2 - w1) x + (u1 - u2) z + (z1 - z2) u + (x2 - x1) w = u1 z1 - x1 w1 - u2 z2 + x2 w2
//   (w3 - w1) x + (u1 - u3) z + (z1 - z3) u + (x3 - x1) w = u1 z1 - x1 w1 - u3 z3 + x3 w3
//   (w4 - w1) x + (u1 - u4) z + (z1 - z4) u + (x4 - x1) w = u1 z1 - x1 w1 - u4 z4 + x4 w4
//
