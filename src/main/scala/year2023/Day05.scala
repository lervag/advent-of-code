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
  lines.drop(1).filterNot(_.size == 0).foreach(_ match {
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
  val seeds = lines(0).split(": ")(1).split(" ").toVector.map(BigInt(_))

  // Solve Part 1
  val part1 = seeds.map(seedToLocation(_, maps)).min
  println(s"Part 1: $part1")

  // Solve Part 2
  val seedRanges = seeds.grouped(2).toVector.map { pair =>
    (pair(0) to pair(0) + pair(1) - 1)
  }.sorted
  val part2 = seedRanges.map { range =>
    println(s"Starting on range: $range")
    val x = range.tail.fold(seedToLocation(range.head, maps)) { (acc, next) =>
      Ordering[BigInt].min(acc, seedToLocation(next, maps))
    }
    x
  }.min
  println(s"Part 2: $part2")
}

private def seedToLocation(
  input: BigInt,
  maps: Map[String, Vector[(Inclusive[BigInt], BigInt)]]): BigInt =
    Array(
      "seed-to-soil",
      "soil-to-fertilizer",
      "fertilizer-to-water",
      "water-to-light",
      "light-to-temperature",
      "temperature-to-humidity",
      "humidity-to-location"
    ).foldLeft(input) { (acc, map) => followMap(acc, maps(map)) }

private def followMap(
  input: BigInt,
  map: Vector[(Inclusive[BigInt], BigInt)]): BigInt =
    map
      .find { case (source, _) => source.contains(input) }
      .map { case (source, diff) => input + diff }
      .getOrElse(input)
