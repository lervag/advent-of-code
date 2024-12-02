package year2024

import scala.io.Source
import cats.parse.{Parser, Numbers}

def day01: Unit = {
  val p_int = Numbers.digits.map(_.toInt)
  val p_ws = Parser.char(' ').rep0.void
  val p_numbers = ((p_int <* p_ws) ~ p_int)

  // val source = Source.fromFile("resources/2024/day-01")
  val source = Source.fromFile("resources/2024/day-01a")
  val lines = source.getLines()
  val (left, right) = lines.toVector.map { line =>
    p_numbers.parseAll(line).getOrElse(0, 0)
  }.unzip
  source.close()

  val sum_sorted_diffs =
    left.sorted
      .zip(right.sorted)
      .map((x, y) => math.abs(x - y))
      .sum
  println(sum_sorted_diffs)

  val similarity_score = left.map { n =>
    right.count(_ == n) * n
  }.sum
  println(similarity_score)
}
