package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.NumericRange.Inclusive

def day05: Unit = {
  val source = Source.fromFile("resources/2023/day-05")
  val lines = source.getLines.toVector
  source.close()

  val seeds = lines(0).split(": ")(1).split(" ").toVector.map(BigInt(_))
  val maps = mutable.Map.empty[
    String,
    ArrayBuffer[(Inclusive[BigInt], Inclusive[BigInt])]
  ]

  var current: String = ""
  lines.drop(1).filterNot(_.size == 0).foreach(_ match {
    case s"$__new map:" =>
      current = __new
      maps(current) = ArrayBuffer.empty[(Inclusive[BigInt], Inclusive[BigInt])]
    case s"$_destination $_source $_length" =>
      val destination = BigInt(_destination)
      val source = BigInt(_source)
      val length = BigInt(_length)
      maps(current) += ((
        (source to source + length - 1),
        (destination to destination + length - 1),
        ))
  })

  val part1 = seeds.map(seedToLocation(_, maps)).tapEach(println).min
  println(s"Part 1: $part1")
}

private def seedToLocation(
  input: BigInt,
  maps: mutable.Map[String, ArrayBuffer[(Inclusive[BigInt], Inclusive[BigInt])]]): BigInt = {
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
      x = followMap(x, map, maps)
    }

    x
}

private def followMap(
  input: BigInt,
  map: String,
  maps: mutable.Map[String, ArrayBuffer[(Inclusive[BigInt], Inclusive[BigInt])]]): BigInt = {
    maps(map).foreach { (sourceRange, destRange) =>
      if (sourceRange.contains(input))
        return destRange.start + input - sourceRange.start
    }
    input
}
