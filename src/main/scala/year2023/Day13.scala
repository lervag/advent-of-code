package year2023

import scala.io.Source
import scala.collection.mutable

def day13: Unit = {
  val source = Source.fromFile("resources/2023/day-13")
  val lines = source.getLines.toVector
  source.close()

  val patterns = lines
    .foldRight(Vector(Vector.empty[String])) { (line, acc) =>
      if (line.isEmpty)
        Vector.empty[String] +: acc
      else
        (line +: acc.head) +: acc.tail
    }
    .filter(_.nonEmpty)
    .map(parsePatterns)

  val part1 = patterns.map { case (rows, _, cols, _) =>
    findReflectionValue(rows, cols)
  }.sum
  println(s"Part 1: $part1")

  val part2 = patterns
    .map { case (rows, rowLen, cols, colLen) =>
      (rows, rowLen, cols, colLen, findReflectionValue(rows, cols))
    }
    .map(findReflectionValueWithSmudge)
    .sum
  println(s"Part 2: $part2")
}

private def parsePatterns(chunk: Vector[String]) = {
  val bits = chunk.map(
    _.replaceAllLiterally(".", "0").replaceAllLiterally("#", "1")
  )
  val rows = bits.map(Integer.parseInt(_, 2))
  val cols = bits.transpose.map { v => Integer.parseInt(v.mkString, 2) }
  (rows, bits.head.size, cols, bits.size)
}

private def findReflectionValue(
    patternRows: Vector[Int],
    patternCols: Vector[Int]
) = {
  val value = findReflection(patternCols)
  if (value > 0)
    value
  else
    100 * findReflection(patternRows)
}

private def findReflection(numbers: Vector[Int], ignore: Int = 0): Int =
  (1 until numbers.size)
    .filterNot(_ == ignore)
    .find { i =>
      val left = numbers.take(i).reverse
      if (left.size > numbers.size - i)
        left.startsWith(numbers.slice(i, numbers.size))
      else
        numbers.slice(i, numbers.size).startsWith(left)
    }
    .getOrElse(0)

private def findReflectionValueWithSmudge(
    patternRows: Vector[Int],
    rowLength: Int,
    patternCols: Vector[Int],
    colLength: Int,
    oldValue: Int
) = {
  val value = findSmudgedReflection(patternCols, colLength, oldValue)
  if (value > 0)
    value
  else
    100 * findSmudgedReflection(patternRows, rowLength, oldValue / 100)
}

private def findSmudgedReflection(
    numbers: Vector[Int],
    numberLength: Int,
    oldValue: Int
): Int =
  (for {
    i <- (0 until numbers.size).view
    j <- (0 until numberLength).view
    newNumbers = numbers.updated(i, numbers(i) ^ (1 << j))
    newValue = findReflection(newNumbers, oldValue)
    if newValue > 0
  } yield newValue).headOption.getOrElse(0)
