  package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

def day04: Unit = {
  val source = Source.fromFile("resources/2023/day-04")
  val lines = source.getLines.toVector
  source.close()

  val cards = lines.map { line =>
    line match {
      case s"Card $n: $winning | $numbers" =>
        (n.trim().toInt,
          winning.split(" +").filter(_.size > 0).map(_.toInt).toSet,
          numbers.split(" +").filter(_.size > 0).map(_.toInt).toSet)
    }
  }

  val part1 = cards.map { (card, winning_numbers, card_numbers) =>
    val appears = card_numbers.intersect(winning_numbers)
    if (appears.size > 0)
      1 << (appears.size - 1)
    else
      0
  }.sum

  println(s"Part 1: $part1")
}
