package year2023

import scala.io.Source

def day07: Unit = {
  val source = Source.fromFile("resources/2023/day-07")
  val lines = source.getLines.toVector
  source.close()

  val hands = lines
    .map { line =>
      val hand = line.substring(0, 5).toVector
      val bid = line.substring(5).trim.toInt
      (hand, bid)
    }

  val scores = Map[Char, Int](
    'A' -> 12,
    'K' -> 11,
    'Q' -> 10,
    'J' -> 9,
    'T' -> 8,
    '9' -> 7,
    '8' -> 6,
    '7' -> 5,
    '6' -> 4,
    '5' -> 3,
    '4' -> 2,
    '3' -> 1,
    '2' -> 0
  )
  val part1 = hands
    .map { (hand, bid) =>
      val rankSum = hand
        .map(scores(_))
        .reverse
        .zipWithIndex
        .foldLeft(0.0) { case (acc, (score, exp)) =>
          acc + score * math.pow(13, exp)
        } + getType(hand) * math.pow(13, 6)

      (rankSum, bid)
    }
    .sorted
    .map(_._2)
    .zipWithIndex
    .map { (b, r) => (r + 1) * b }
    .sum
  println(s"Part 1: $part1")

  val scoresNew = Map[Char, Int](
    'A' -> 12,
    'K' -> 11,
    'Q' -> 10,
    'T' -> 9,
    '9' -> 8,
    '8' -> 7,
    '7' -> 6,
    '6' -> 5,
    '5' -> 4,
    '4' -> 3,
    '3' -> 2,
    '2' -> 1,
    'J' -> 0
  )
  val part2 = hands
    .map { (hand, bid) =>
      val rankSum = hand
        .map(scoresNew(_))
        .reverse
        .zipWithIndex
        .foldLeft(0.0) { case (acc, (score, exp)) =>
          acc + score * math.pow(13, exp)
        } + getTypeJoker(hand) * math.pow(13, 6)

      (rankSum, bid)
    }
    .sorted
    .map(_._2)
    .zipWithIndex
    .map { (b, r) => (r + 1) * b }
    .sum
  println(s"Part 2: $part2")
}

private def getTypeJoker(hand: Vector[Char]): Int = {
  val jokers = hand.filter(_ == 'J').size
  val map = hand
    .filter(_ != 'J')
    .groupBy(identity)
    .map { (c, v) => (c, v.size) }
  val counts = map.values.toVector

  (map.size, jokers) match {
    case (0, 5)          => 6
    case (1, j)          => 6
    case (2, j) if j > 1 => 5
    case (2, j) if j <= 1 =>
      if (map.values.exists(_ == 4 - j))
        5
      else
        4
    case (3, j) if j > 0 => 3
    case (3, 0) =>
      if (map.values.exists(_ == 3))
        3
      else
        2
    case (4, j) => 1
    case (5, 0) => 0
  }
}

private def getType(hand: Vector[Char]): Int = {
  val map = hand.groupBy(identity).map { (c, v) => (c, v.size) }
  map.size match {
    case 1 => 6
    case 2 =>
      if (map.values.exists(_ == 4))
        5
      else
        4
    case 3 =>
      if (map.values.exists(_ == 3))
        3
      else
        2
    case 4 => 1
    case 5 => 0
  }
}
