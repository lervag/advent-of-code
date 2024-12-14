  package year2023

import scala.collection.mutable
import scala.io.Source

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

  val counter = mutable.Map.empty[Int, Int].withDefaultValue(0)
  val part1 = cards.map { (card, winning_numbers, card_numbers) =>
    // Count number of cards for part 2
    counter(card) += 1
    (1 to card_numbers.intersect(winning_numbers).size).foreach { i =>
      counter(card + i) += counter(card)
    }

    // Score for part 1
    val n = card_numbers.intersect(winning_numbers).size - 1
    if (n >= 0)
      1 << n
    else
      0
  }.sum

  println(s"Part 1: $part1")
  println(s"Part 2: ${counter.values.sum}")
}
