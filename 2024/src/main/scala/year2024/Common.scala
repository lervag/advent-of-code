package year2024

type Point = (Int, Int)

extension (a: Point) {
  def +(b: Point): Point = (a._1 + b._1, a._2 + b._2)

  def -(b: Point): Point = (a._1 - b._1, a._2 - b._2)

  def distanceTo(b: Point) =
    math.abs(a._1 - b._1) + math.abs(a._2 - b._2)

  def isAdjacentTo(b: Point): Boolean =
    (math.abs(a._1 - b._1) == 1 && a._2 == b._2)
      || (a._1 == b._1 && math.abs(a._2 - b._2) == 1)

  def isInGrid(nx: Int, ny: Int): Boolean =
    a._1 >= 0 && a._1 < nx && a._2 >= 0 && a._2 < ny
}

case class Position(i: Int, j: Int)

def egcd(m: Int, n: Int) = egcdaux(1, 0, 0, 1, m, n)

private def egcdaux(
    a1: Int,
    b1: Int,
    a: Int,
    b: Int,
    c: Int,
    d: Int
): (Int, Int, Int) = {
  val q = c / d
  val r = c % d
  if (r == 0)
    (a, b, d)
  else egcdaux(a, b, a1 - q * a, b1 - q * b, d, r)
}
