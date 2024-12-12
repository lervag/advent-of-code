package year2024

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.MapView

def day12: Unit = {
  val source = Source.fromFile("resources/2024/day-12a")
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

private def calculateSides(region: Set[Point]): Int =
  Vector((1, 0), (0, -1), (-1, 0), (0, 1)).flatMap { d =>
    region.toVector
      .collect { case p if !region.contains(p + d) => p }
      .groupMap
        { p => if d._1 == 0 then p._2 else p._1 }
        { p => if d._1 == 0 then p._1 else p._2 }
      .view
      .mapValues(countAdjacentGroups)
      .values
  }.sum

private def countAdjacentGroups(numbers: Vector[Int]): Int =
  numbers.sorted
    .foldLeft(0, Option.empty[Int]) {
      case ((count, None), n) =>
        (count + 1, Some(n))
      case ((count, Some(prev)), n) =>
        if (prev + 1 == n) (count, Some(n))
        else (count + 1, Some(n))
    }
    ._1
