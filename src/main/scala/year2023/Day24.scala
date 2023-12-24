package year2023

import scala.io.Source
import scala.collection.mutable

import breeze.linalg.{DenseVector, DenseMatrix, inv}
import breeze.linalg.norm
import breeze.linalg.pinv
import breeze.optimize.DiffFunction
import breeze.linalg.sum
import breeze.optimize.LBFGS

def day24(): Unit = {
  val source = Source.fromFile("resources/2023/day-24a")
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

  // Find stone (represented as a hail) H such that
  //
  //   dt = collision(H, h) > 0 for all h in hails
  //
  // We need three known hails h. This gives 9 equations with 9 unknowns, that
  // we solve with an iterative method.
  //
  // Written out, the idea is to solve:
  //
  //   (x - h1.x) + t1 * (u - h1.u) = 0
  //   (x - h2.x) + t2 * (u - h2.u) = 0
  //   (x - h3.x) + t3 * (u - h3.u) = 0
  //   (y - h1.y) + t1 * (v - h1.v) = 0
  //   (y - h2.y) + t2 * (v - h2.v) = 0
  //   (y - h3.y) + t3 * (v - h3.v) = 0
  //   (z - h1.z) + t1 * (w - h1.w) = 0
  //   (z - h2.z) + t2 * (w - h2.w) = 0
  //   (z - h3.z) + t3 * (w - h3.w) = 0
  val part2 = iterativeSolver(hails(0), hails(1), hails(2))
  println(s"Part 2: $part2")
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

private def iterativeSolver(h1: Hail, h2: Hail, h3: Hail) = {
  // Written out, the idea is to solve:
  //
  //   (x - h1.x) + t1 * (u - h1.u) = 0
  //   (x - h2.x) + t2 * (u - h2.u) = 0
  //   (x - h3.x) + t3 * (u - h3.u) = 0
  //   (y - h1.y) + t1 * (v - h1.v) = 0
  //   (y - h2.y) + t2 * (v - h2.v) = 0
  //   (y - h3.y) + t3 * (v - h3.v) = 0
  //   (z - h1.z) + t1 * (w - h1.w) = 0
  //   (z - h2.z) + t2 * (w - h2.w) = 0
  //   (z - h3.z) + t3 * (w - h3.w) = 0
  //
  // Given a guess of x, y, z, u, v, w we find:
  //
  //   t1 = (u - h1.u)/(x - h1.x)
  //   t2 = (u - h2.u)/(x - h2.x)
  //   t3 = (u - h3.u)/(x - h3.x)
  //
  // We can take the mean t1, t2, t3. Now we find an updated guess, and we
  // rince and repeat.
  var x = 1000
  var y = 200
  var z = 30
  var u = 10
  var v = 20
  var w = 30
  var iter = 0
  val tol = 1e-6
  val maxIterations = 100
  var converged = false
  while (iter < maxIterations && !converged) {
    val t1 = 0.333333333*(
        (u - h1.u) / (x - h1.x)
      + (v - h1.v) / (y - h1.y)
      + (w - h1.w) / (z - h1.z)
    )
    val t2 = 0.333333333*(
        (u - h2.u) / (x - h2.x)
      + (v - h2.v) / (y - h2.y)
      + (w - h2.w) / (z - h2.z)
    )
    val t3 = 0.333333333*(
        (u - h3.u) / (x - h3.x)
      + (v - h3.v) / (y - h3.y)
      + (w - h3.w) / (z - h3.z)
    )

    u = (((h1.x - h2.x) + t1 * h1.u - t2 * h2.u)/(t1 - t2)).toInt
    x = (h1.x - t1 * (u - h1.u)).toInt

    v = (((h2.y - h3.y) + t2 * h2.v - t3 * h3.v)/(t2 - t3)).toInt
    y = (h2.y - t2 * (v - h2.v)).toInt

    w = (((h3.z - h1.z) + t3 * h3.w - t1 * h1.w)/(t3 - t1)).toInt
    z = (h3.z - t3 * (w - h3.w)).toInt

    val res = Vector(
       (x - h1.x) + t1 * (u - h1.u),
       (x - h2.x) + t2 * (u - h2.u),
       (x - h3.x) + t3 * (u - h3.u),
       (y - h1.y) + t1 * (v - h1.v),
       (y - h2.y) + t2 * (v - h2.v),
       (y - h3.y) + t3 * (v - h3.v),
       (z - h1.z) + t1 * (w - h1.w),
       (z - h2.z) + t2 * (w - h2.w),
       (z - h3.z) + t3 * (w - h3.w)
     ).map { r => 0.5*r*r }.sum

    println(res)

    // if (delta < tol) {
    //   converged = true
    // }
    iter += 1
  }

  x + y + z
}

private def newtonSolver(h1: Hail, h2: Hail, h3: Hail) = {
  val objectiveFunction = new DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      val residual = F(x, h1, h2, h3)
      val value = 0.5 * sum(residual *:* residual)
      (value, residual)
    }
  }

  val x0 = DenseVector(24.0, 13.0, 10.0, -3.0, 1.0, 2.0, 1.0, 1.0, 2.0)
  val optimizer = new LBFGS[DenseVector[Double]](maxIter = 400, m = 3)
  val result = optimizer.minimize(objectiveFunction, x0)
  println(result)

  h1
}

private def F(
    x: DenseVector[Double],
    h1: Hail,
    h2: Hail,
    h3: Hail
) = DenseVector(
  (x(0) - h1.x) + x(6) * (x(3) - h1.u),
  (x(1) - h1.y) + x(6) * (x(4) - h1.v),
  (x(2) - h1.z) + x(6) * (x(5) - h1.w),
  (x(0) - h2.x) + x(7) * (x(3) - h2.u),
  (x(1) - h2.y) + x(7) * (x(4) - h2.v),
  (x(2) - h2.z) + x(7) * (x(5) - h2.w),
  (x(0) - h3.x) + x(8) * (x(3) - h3.u),
  (x(1) - h3.y) + x(8) * (x(4) - h3.v),
  (x(2) - h3.z) + x(8) * (x(5) - h3.w)
)

private def jacobianF(
    x: DenseVector[Double],
    h1: Hail,
    h2: Hail,
    h3: Hail
): DenseMatrix[Double] = DenseMatrix(
  (1.0, 0.0, 0.0, x(6), 0.0, 0.0, (x(3) - h1.u), 0.0, 0.0),
  (0.0, 1.0, 0.0, 0.0, x(6), 0.0, (x(4) - h1.v), 0.0, 0.0),
  (0.0, 0.0, 1.0, 0.0, 0.0, x(6), (x(5) - h1.w), 0.0, 0.0),
  (1.0, 0.0, 0.0, x(7), 0.0, 0.0, 0.0, (x(3) - h2.u), 0.0),
  (0.0, 1.0, 0.0, 0.0, x(7), 0.0, 0.0, (x(4) - h2.v), 0.0),
  (0.0, 0.0, 1.0, 0.0, 0.0, x(7), 0.0, (x(5) - h2.w), 0.0),
  (1.0, 0.0, 0.0, x(8), 0.0, 0.0, 0.0, 0.0, (x(3) - h2.u)),
  (0.0, 1.0, 0.0, 0.0, x(8), 0.0, 0.0, 0.0, (x(4) - h2.v)),
  (0.0, 0.0, 1.0, 0.0, 0.0, x(8), 0.0, 0.0, (x(5) - h2.w))
)
