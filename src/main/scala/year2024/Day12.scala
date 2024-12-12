package year2024

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.MapView

def day12: Unit = {
  val source = Source.fromFile("resources/2024/day-12")
  val regions = source
    .getLines()
    .toVector
    .zipWithIndex
    .flatMap { (l, j) => l.toVector.zipWithIndex.map { (p, i) => (p, (i, j)) } }
    .groupMap(_._1) { case (_, (i, j)) => (i, j) }
    .view
    .mapValues(intoRegions)
  source.close()

  extension (m: MapView[Char, Vector[(Int, Int)]])
    def price: Int = m.values.flatten.map(_ * _).sum

  val part1 = regions
    .mapValues(_.map { region => (region.size, calculatePerimiter(region)) })
    .price
  println(part1)

  val part2 = regions
    .filter(_._1 == 'F')
    .mapValues(_.map { region => (region.size, calculateSides(region)) })
    .price
  println(part2)
}

private def intoRegions(v: Vector[Point]): Vector[Set[Point]] =
  v.foldLeft(Vector(mutable.Set[Point]())) { (acc, p) =>
    val fitsInSets = ArrayBuffer[Int]()
    acc.zipWithIndex.foreach { (s, i) =>
      if s.isEmpty || s.exists(p.isAdjacentTo(_)) then
        fitsInSets.addOne(i)
        s.addOne(p)
    }
    if fitsInSets.size > 1 then
      val merged = fitsInSets.map(acc(_)).reduce((s1, s2) => s1 ++ s2)
      fitsInSets.foldRight(acc) { (i, a) => a.patch(i, Nil, 1) } :+ merged
    else if fitsInSets.isEmpty then acc :+ mutable.Set(p)
    else acc
  }.map(_.toSet)

private def calculatePerimiter(region: Set[Point]): Int =
  region.toVector.map { p => 4 - region.count(p.isAdjacentTo(_)) }.sum

private def calculateSides(region: Set[Point]): Int = {
  // 12          4
  // AAAAAA  4   AAAA
  // AAA  A  BB
  // AAA  A  BB   8     4
  // A  AAA       C     D
  // A  AAA       CC
  // AAAAAA        C

  region.toVector.map { p =>

    val adjacents = region.filter(p.isAdjacentTo(_))
  }

  0
}
