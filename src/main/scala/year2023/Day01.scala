package year2023

import scala.io.Source

def day01: Unit = {
  val source = Source.fromFile("resources/2023/day-01")
  val lines = source.getLines().toVector
  source.close()

  val wordToDigit = Map(
    "one"   -> 1,
    "two"   -> 2,
    "three" -> 3,
    "four"  -> 4,
    "five"  -> 5,
    "six"   -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine"  -> 9
  )

  val regexDigitF = "(one|two|three|four|five|six|seven|eight|nine|[0-9])".r
  val regexDigitB = "(eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9])".r

  val calibration_values = lines.map { line =>
    val d1S = regexDigitF.findFirstIn(line).head
    val d1 = if (d1S.size > 1)
        wordToDigit(d1S)
      else
        d1S.toInt

    val d2S = regexDigitB.findFirstIn(line.reverse).head.reverse
    val d2 = if (d2S.size > 1)
        wordToDigit(d2S)
      else
        d2S.toInt

    s"${d1}${d2}".toInt
  }.toVector

  println(s"The sum of all calibration values: ${calibration_values.sum}")
}
