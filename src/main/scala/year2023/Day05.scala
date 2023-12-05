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
    ArrayBuffer[(Inclusive[BigInt], Inclusive[BigInt])]
  ]
  var current: String = ""
  lines.drop(1).filterNot(_.size == 0).foreach(_ match {
    case s"$__new map:" =>
      current = __new
      mapBuilder(current) = ArrayBuffer.empty[(Inclusive[BigInt], Inclusive[BigInt])]
    case s"$_destination $_source $_length" =>
      val destination = BigInt(_destination)
      val source = BigInt(_source)
      val length = BigInt(_length)
      mapBuilder(current) += ((
        (source to source + length - 1),
        (destination to destination + length - 1),
        ))
  })

  val maps = mapBuilder.toMap.map { (name, array) => name -> array.toVector }
  val seeds = lines(0).split(": ")(1).split(" ").toVector.map(BigInt(_))

  // Solve Part 1
  val part1 = seeds.map(seedToLocation(_, maps)).min
  println(s"Part 1: $part1")

  // Solve Part 2
  val seedRanges = seeds.grouped(2).toVector.map { pair =>
    (pair(0) to pair(0) + pair(1) - 1)
  }.sorted
  println(s"There are ${seedRanges.size} ranges.")
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
  maps: Map[String, Vector[(Inclusive[BigInt], Inclusive[BigInt])]]): BigInt = {
    var x: BigInt = input

    Array(
      "seed-to-soil",
      "soil-to-fertilizer",
      "fertilizer-to-water",
      "water-to-light",
      "light-to-temperature",
      "temperature-to-humidity",
      "humidity-to-location"
    ).foreach { map =>
      x = followMap(x, maps(map))
    }

    x
}

private def followMap(
  input: BigInt,
  map: Vector[(Inclusive[BigInt], Inclusive[BigInt])]): BigInt = {
    map.foreach { (sourceRange, destRange) =>
      if (sourceRange.contains(input))
        return destRange.start + input - sourceRange.start
    }
    input
}
