package year2023

import scala.io.Source

def day01: Unit = {
  val source = Source.fromFile("resources/2023/day-01a")
  val calibration_values = source.getLines().map { line =>
    digits = line.filter(isDigit)
    d1 = digits.first.toInt
    d2 = digits.last.toInt
    d1 + d2
  }
  source.close()

  println(s"The sum of all calibration values: ${calibration_values.sum()}")
}
