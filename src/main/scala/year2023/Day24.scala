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
  def F(
      x: DenseVector[Long],
      h1: Hail,
      h2: Hail,
      h3: Hail
  ) = DenseVector(
    (h1.v - h2.v)*x(0) + (h2.u - h1.u)*x(1) + (h2.y - h1.y)*x(3) - (h2.x - h1.x)*x(4) + h1.u*h1.y - h1.x*h1.v - h2.u*h2.y + h2.x*h2.v,
    (h1.v - h3.v)*x(0) + (h3.u - h1.u)*x(1) + (h3.y - h1.y)*x(3) - (h3.x - h1.x)*x(4) + h1.u*h1.y - h1.x*h1.v - h3.u*h3.y + h3.x*h3.v,
    (h2.v - h3.v)*x(0) + (h3.u - h2.u)*x(1) + (h3.y - h2.y)*x(3) - (h3.x - h2.x)*x(4) + h2.u*h2.y - h2.x*h2.v - h3.u*h3.y + h3.x*h3.v,
    (h1.w - h2.w)*x(0) + (h2.u - h1.u)*x(2) + (h2.z - h1.z)*x(3) - (h2.x - h1.x)*x(5) + h1.u*h1.z - h1.x*h1.w - h2.u*h2.z + h2.x*h2.w,
    (h1.w - h3.w)*x(0) + (h3.u - h1.u)*x(2) + (h3.z - h1.z)*x(3) - (h3.x - h1.x)*x(5) + h1.u*h1.z - h1.x*h1.w - h3.u*h3.z + h3.x*h3.w,
    (h2.w - h3.w)*x(0) + (h3.u - h2.u)*x(2) + (h3.z - h2.z)*x(3) - (h3.x - h2.x)*x(5) + h2.u*h2.z - h2.x*h2.w - h3.u*h3.z + h3.x*h3.w
  )

  val jacobian = DenseMatrix(
    ((h1.v - h2.v), (h1.v - h3.v), (h2.v - h3.v), (h1.w - h2.w), (h1.w - h3.w), (h2.w - h3.w)),
    ((h2.u - h1.u), (h3.u - h1.u), (h3.u - h2.u), 0L, 0L, 0L),
    (0L, 0L, 0L, (h2.u - h1.u), (h3.u - h1.u), (h3.u - h2.u)),
    ((h2.y - h1.y), (h3.y - h1.y), (h3.y - h2.y), (h2.z - h1.z), (h3.z - h1.z), (h3.z - h2.z)),
    ((h2.x - h1.x), (h3.x - h1.x), (h3.x - h2.x), 0L, 0L, 0L),
    (0L, 0L, 0L, (h2.x - h1.x), (h3.x - h1.x), (h3.x - h2.x))
  ).t

  var x = DenseVector(24L, 13L, 10L, -3L, 1L, 2L)

//   println(jacobian)
//   println(rank(jacobian))
  val invJ = -inv(jacobian)
//   println(invJ)

  var converged = false
  var iter = 0
  while (iter < 10 && !converged) {
    val Fx = F(x, h1, h2, h3)
    val delta = invJ * Fx
    // println(Fx)
    println(norm(delta))
    x += delta
    if (norm(delta) < 1e-6) {
      converged = true
    }
    iter += 1
  }

  // println(x)
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
//   (x - h1.x) + t1 * (u - h1.u) = 0
//   (y - h1.y) + t1 * (v - h1.v) = 0
//   (z - h1.z) + t1 * (w - h1.w) = 0
//   (x - h2.x) + t2 * (u - h2.u) = 0
//   (y - h2.y) + t2 * (v - h2.v) = 0
//   (z - h2.z) + t2 * (w - h2.w) = 0
//   (x - h3.x) + t3 * (u - h3.u) = 0
//   (y - h3.y) + t3 * (v - h3.v) = 0
//   (z - h3.z) + t3 * (w - h3.w) = 0
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
//   (v1 - v2) x + (u2 - u1) y + (y2 - y1) u - (x2 - x1) v + u1 y1 - x1 v1 - u2 y2 + x2 v2 = 0
//   (v1 - v3) x + (u3 - u1) y + (y3 - y1) u - (x3 - x1) v + u1 y1 - x1 v1 - u3 y3 + x3 v3 = 0
//   (v2 - v3) x + (u3 - u2) y + (y3 - y2) u - (x3 - x2) v + u2 y2 - x2 v2 - u3 y3 + x3 v3 = 0
//   (w1 - w2) x + (u2 - u1) z + (z2 - z1) u - (x2 - x1) w + u1 z1 - x1 w1 - u2 z2 + x2 w2 = 0
//   (w1 - w3) x + (u3 - u1) z + (z3 - z1) u - (x3 - x1) w + u1 z1 - x1 w1 - u3 z3 + x3 w3 = 0
//   (w2 - w3) x + (u3 - u2) z + (z3 - z2) u - (x3 - x2) w + u2 z2 - x2 w2 - u3 z3 + x3 w3 = 0
//
// These are now 6 linear equations with 6 variables x, y, z, u, v, w.
