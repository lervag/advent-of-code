package year2022

import scala.io.Source

def day01: Unit = {
  val source = Source.fromFile("resources/input-day-01")
  val input = source.mkString
  source.close()

  val mylist = input
    .split("\n\n")
    .map(_.split("\n").map(_.toInt))
    .zipWithIndex

  val x = mylist
    .map { (list, i) =>
      (i, list.length, list.sum)
    }
    .sortBy(_._3)(Ordering.Int.reverse)

  println(s"The elf is number ${x(0)._1} carrying ${x(0)._3}")

  val y = x.take(3).map(_._3).sum
  println(s"The top three elves carry: $y")
}
