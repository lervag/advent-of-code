import year2024._

/** Usage: sbt "run [day]" (defaults to the last solved day) */
@main def main(args: String*): Unit = {
  val day = args.headOption.map(_.toInt).getOrElse(25)
  val before = System.currentTimeMillis
  day match {
    case 1 => day01
    case 2 => day02
    case 3 => day03
    case 4 => day04
    case 5 => day05
    case 6 => day06
    case 7 => day07
    case 8 => day08
    case 9 => day09
    case 10 => day10
    case 11 => day11
    case 12 => day12
    case 13 => day13
    case 14 => day14
    case 15 => day15
    case 16 => day16
    case 17 => day17
    case 18 => day18
    case 19 => day19
    case 20 => day20
    case 21 => day21
    case 22 => day22
    case 23 => day23
    case 24 => day24
    case 25 => day25
    case _ => println(s"No solution for day $day")
  }
  val after = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
}
