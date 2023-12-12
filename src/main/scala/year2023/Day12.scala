package year2023

import scala.io.Source
import scala.collection.mutable

def day12: Unit = {
  val source = Source.fromFile("resources/2023/day-12")
  val lines = source.getLines.toVector
  source.close()

  val records = lines.map { line =>
    val parts = line.split(" ")
    val groupSizes = parts(1).split(",").toVector.map(_.toInt)
    (parts(0), groupSizes)
  }

  val part1 = records.map { (s, g) => countArrangements(s, -1, g, 0) }.sum
  println(s"Part 1: $part1")

  val unfolded = records.map { (s, g) =>
    (Vector.fill(5)(s).mkString("?"), Vector.fill(5)(g).flatten)
  }
  val part2 = unfolded.map { (s, g) => countArrangements(s, -1, g, 0) }.sum
  println(s"Part 2: $part2")
}

private val cache = mutable.Map.empty[(String, Int, Vector[Int], Int), BigInt]

private def countArrangements(springs: String, currentNum: Int, remainingGroups: Vector[Int], i: Int): BigInt =
  cache.getOrElseUpdate(
    (springs, currentNum, remainingGroups, i),
    countArrangementsAux(springs, currentNum, remainingGroups, i)
  )

private def countArrangementsAux(springs: String, currentNum: Int, remainingGroups: Vector[Int], i: Int): BigInt = {
  if (i >= springs.length)
    if (remainingGroups.isEmpty && currentNum <= 0)
      1
    else
      0
  else if (remainingGroups.isEmpty && currentNum <= 0)
    if (springs.substring(i).exists(_ == '#'))
      0
    else
      1
  else
    springs(i) match {
      case '#' =>
        if (currentNum == 0)
          0
        else if (currentNum == -1)
          countArrangements(springs, remainingGroups.head - 1, remainingGroups.tail, i + 1)
        else
          countArrangements(springs, currentNum - 1, remainingGroups, i + 1)
      case '.' =>
        if (currentNum > 0)
          0
        else
          countArrangements(springs, -1, remainingGroups, i + 1)
      case '?' =>
        if (currentNum > 0)
          countArrangements(springs, currentNum - 1, remainingGroups, i + 1)
        else if (currentNum == 0)
          countArrangements(springs, -1, remainingGroups, i + 1)
        else
          val dot = countArrangements(springs, -1, remainingGroups, i + 1)
          val hash = countArrangements(springs, remainingGroups.head - 1, remainingGroups.tail, i + 1)
          dot + hash
    }
}
