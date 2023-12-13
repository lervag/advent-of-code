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

  val part1 = patterns
    .map { case (rows, _, cols, _) => findReflectionValue(rows, cols) }
    .sum
  println(s"Part 1: $part1")

  val part2 = patterns
    .map { case (rows, rowLen, cols, colLen) =>
      (rows, rowLen, cols, colLen, findReflectionValue(rows, cols))
    }
    .map(findReflectionValueWithSmudge)
    .sum
  println(s"Part 2: $part2")
}

private def findReflectionValueWithSmudge(
    patternRows: Vector[Int],
    rowLength: Int,
    patternCols: Vector[Int],
    colLength: Int,
    currentVal: Int
) = {
  val newVal = findSmudge(patternCols, colLength, currentVal)
  if (newVal > 0)
    newVal
  else
    findSmudge(patternRows, rowLength, currentVal, 100)
}

private def findSmudge(
    numbers: Vector[Int],
    numberLength: Int,
    oldValue: Int,
    factor: Int = 1,
): Int = {
  var i = 0
  while (i < numbers.size) {
    var j = 0
    while (j < numberLength) {
      val newNumbers = flipBit(numbers, i, j)
      val newValue = findReflection(newNumbers, factor, oldValue)
      if (newValue > 0)
        return newValue
      j += 1
    }
    i += 1
  }

  0
}

private def flipBit(numbers: Vector[Int], i: Int, k: Int): Vector[Int] =
  numbers.updated(i, numbers(i) ^ (1 << k))

private def findReflectionValue(
    patternRows: Vector[Int],
    patternCols: Vector[Int]
) = {
  val value = findReflection(patternCols)
  if (value > 0)
    value
  else
    findReflection(patternRows, 100)
}

private def findReflection(numbers: Vector[Int], factor: Int = 1, ignore: Int = 0): Int =
  (1 until numbers.size)
    .filterNot(_ == ignore/factor)
    .find { i =>
      val left = numbers.take(i).reverse
      if (left.size > numbers.size - i)
        left.startsWith(numbers.slice(i, numbers.size))
      else
        numbers.slice(i, numbers.size).startsWith(left)
    }
    .map(_*factor)
    .getOrElse(0)

private def parsePatterns(chunk: Vector[String]) = {
  val bits = chunk.map(
    _.replaceAllLiterally(".", "0")
      .replaceAllLiterally("#", "1")
  )
  val rows = bits.map(Integer.parseInt(_, 2))
  val cols = bits.transpose.map { v => Integer.parseInt(v.mkString, 2) }
  (rows, bits.head.size, cols, bits.size)
}
