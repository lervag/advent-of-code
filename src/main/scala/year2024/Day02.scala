package year2024

import scala.io.Source
import cats.parse.{Parser, Numbers}

def day02: Unit = {
  val p_int = Numbers.digits.map(_.toInt)
  val p_ws = Parser.char(' ').rep0.void
  val p_report = Parser.repSep0(p_int, p_ws).map(_.toVector)

  // val source = Source.fromFile("resources/2024/day-02")
  val source = Source.fromFile("resources/2024/day-02a")
  val lines = source.getLines()
  val reports = lines.toVector.map { line =>
    p_report.parseAll(line).getOrElse(Vector[Int]())
  }
  source.close()

  println(reports.count(is_safe))

  println(reports.count(is_safe_with_dampener))
}

private def is_safe(report: Vector[Int]): Boolean = {
  val diffs = report
    .sliding(2)
    .map {
      case Vector(x, y) => x - y
      case _ => 0
    }
    .toVector
  val too_large = diffs.count(x => math.abs(x) < 1 || math.abs(x) > 3)
  val same_sign = diffs.count(_ > 0)
  println((report, diffs, too_large, same_sign))

  too_large == 0 && (same_sign == 0 || same_sign == diffs.length)
}

private def is_safe_with_dampener(report: Vector[Int]): Boolean = {
  if is_safe(report) then true
  else {
    var i = 0
    var success = false
    while (!success && i < report.size) {
      val reduced_report =
        report.take(i) ++ report.takeRight(report.size - i - 1)
      i += 1
      success = is_safe(reduced_report)
    }

    success
  }
}
