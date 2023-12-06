package year2023

import scala.io.Source
import scala.math.Ordering.Implicits._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.NumericRange.Inclusive
import scala.collection.immutable.NumericRange

def day06: Unit = {
  val source = Source.fromFile("resources/2023/day-06a")
  val lines = source.getLines.toVector
  source.close()

  // We want to find the range of charging values for each game 
  //
  // C^2 - TC + L = 0
  // C = (T ± sqrt(T^2 - 4L)) / 2

  val T = lines(0).substring(10).trim.split(" +").toVector.map(_.toInt)
  val D = lines(1).substring(10).trim.split(" +").toVector.map(_.toInt)
  val games = T.zip(D)

  val part1 = games.map { (t, d) =>
    val c1 = 0.5*(t - math.sqrt(t*t - 4*d)) + 1e-9
    val c2 = 0.5*(t + math.sqrt(t*t - 4*d)) - 1e-9
    (c1.ceil.toInt to c2.toInt)
  }
    .map(_.size)
    .product
  println(s"Part 1: $part1")

  // val T2 = BigInt(lines(0).substring(10).replace(" ", "").toInt)
  // val D2 = BigInt(lines(1).substring(10).replace(" ", "").toInt)
  // val C1 = 0.5*(T2 - math.sqrt(T2*T2 - 4*D2)) + 1e-9
  // val C2 = 0.5*(T2 + math.sqrt(T2*T2 - 4*D2)) - 1e-9
  // println(s"Part 2: ${C2 - C1}")
}
