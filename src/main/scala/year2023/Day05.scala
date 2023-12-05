package year2023

import scala.io.Source
import scala.math.Ordering.Implicits._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.NumericRange.Inclusive
import scala.collection.immutable.NumericRange

def day05: Unit = {
  val source = Source.fromFile("resources/2023/day-05")
  val lines = source.getLines.toVector
  source.close()

  // Parse the input data
  val mapBuilder = mutable.Map.empty[
    String,
    ArrayBuffer[(Inclusive[BigInt], BigInt)]
  ]
  var current: String = ""
  lines
    .drop(1)
    .filterNot(_.size == 0)
    .foreach(_ match {
      case s"$__new map:" =>
        current = __new
        mapBuilder(current) = ArrayBuffer.empty[(Inclusive[BigInt], BigInt)]
      case s"$_destination $_source $_length" =>
        val source = BigInt(_source)
        mapBuilder(current) += ((
          (source to source + BigInt(_length) - 1),
          BigInt(_destination) - source,
        ))
    })

  val maps = mapBuilder.toMap.map { (n, a) => n -> a.toVector.sorted }
  val mapsOrdered = Vector(
    "seed-to-soil",
    "soil-to-fertilizer",
    "fertilizer-to-water",
    "water-to-light",
    "light-to-temperature",
    "temperature-to-humidity",
    "humidity-to-location"
  ).map { map => maps(map) }
  val seeds = lines(0).split(": ")(1).split(" ").toVector.map(BigInt(_))

  // Solve Part 1
  val part1 = seeds.map(seedToLocation(_, mapsOrdered)).min
  println(s"Part 1: $part1")

  // Solve Part 2
  val part2 = seeds
    .grouped(2)
    .toVector
    .map { pair => (pair(0) to pair(0) + pair(1) - 1) }
    .sorted
    .map { range => seedsToMinLocation(range, mapsOrdered) }
    .min
  println(s"Part 2: $part2")
}

private def seedToLocation(
    input: BigInt,
    mapsOrdered: Vector[Vector[(Inclusive[BigInt], BigInt)]]
): BigInt =
  mapsOrdered.foldLeft(input) { (acc, map) => followMap(acc, map) }

private def followMap(
    input: BigInt,
    map: Vector[(Inclusive[BigInt], BigInt)]
): BigInt =
  map
    .find { case (source, _) => source.contains(input) }
    .map { case (source, diff) => input + diff }
    .getOrElse(input)

private def seedsToMinLocation(
    input: Inclusive[BigInt],
    mapsOrdered: Vector[Vector[(Inclusive[BigInt], BigInt)]]
): BigInt =
  mapsOrdered
    .foldLeft(Set(input)) { (acc, map) => println(acc.size); followMapRangeSet(acc, map) }
    .map(_.min)
    .min

private def followMapRangeSet(
    input: Set[Inclusive[BigInt]],
    map: Vector[(Inclusive[BigInt], BigInt)]
): Set[Inclusive[BigInt]] = {
  input
    .map { seedRange =>
      println("Working ...")
      val (outSets, seedsRemaining) =
        map.foldLeft((Set.empty[Inclusive[BigInt]], seedRange)) {
          case ((outSets, seedsRemaining), (source, diff)) =>
            val intersection = rangeIntersection(seedsRemaining, source)
            if (intersection.size > 0) {
              (
                outSets + intersection.addOffset(diff),
                rangeMinus(seedsRemaining, intersection)
              )
            } else {
              (outSets, seedsRemaining)
            }
        }

      if (seedsRemaining.size > 0)
        outSets + seedsRemaining
      else
        outSets
    }
    .foldLeft(Set.empty[Inclusive[BigInt]]) { (acc, nextSet) =>
      acc.union(nextSet)
    }
}

implicit class ExtendedInclusiveRange(range: Inclusive[BigInt]) {
  def addOffset(offset: BigInt): Inclusive[BigInt] = {
    Inclusive(range.start + offset, range.end + offset, 1)
  }
}

private def rangeIntersection(
    r1: Inclusive[BigInt],
    r2: Inclusive[BigInt]
): Inclusive[BigInt] = {
  val start = r1.start.max(r2.start)
  val end = r1.end.min(r2.end)
  start to end
}

private def rangeMinus(
    r1: Inclusive[BigInt],
    r2: Inclusive[BigInt]
): Inclusive[BigInt] = {
  val start = if (r2.contains(r1.start)) r2.end + 1 else r1.start
  val end = if (r2.contains(r1.end)) r2.start - 1 else r1.end
  start to end
}
