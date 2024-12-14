package year2023

import scala.io.Source

def day07: Unit = {
  val source = Source.fromFile("resources/2023/day-07")
  val lines = source.getLines.toVector
  source.close()

  val scores = Map(
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
  val part1 = calculateWinnings(lines, scores, getType)
  println(s"Part 1: $part1")

  val scoresJoker = Map(
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
  val part2 = calculateWinnings(lines, scoresJoker, getTypeJoker)
  println(s"Part 2: $part2")
}

private def calculateWinnings(
    lines: Vector[String],
    scores: Map[Char, Int],
    getTypeFunc: Vector[Char] => Int
): Int = {
  val hands = lines.map { line =>
    val hand = line.take(5).toVector
    val bid = line.drop(5).trim.toInt
    (hand, bid)
  }

  val part = hands
    .map { case (hand, bid) =>
      val rankSum = hand
        .map(scores(_))
        .reverse
        .zipWithIndex
        .foldLeft(0.0) { case (acc, (score, exp)) =>
          acc + score * math.pow(13, exp)
        } + getTypeFunc(hand) * math.pow(13, 6)

      (rankSum, bid)
    }
    .sorted
    .map(_._2)
    .zipWithIndex
    .map { case (b, r) => (r + 1) * b }
    .sum

  part
}

private def getType(hand: Vector[Char]): Int = {
  val map = hand.groupBy(identity).view.mapValues(_.size).toVector
  map.size match {
    case 1 => 6
    case 2 =>
      if (map.map(_._2).contains(4))
        5
      else
        4
    case 3 =>
      if (map.map(_._2).contains(3))
        3
      else
        2
    case 4 => 1
    case 5 => 0
  }
}

private def getTypeJoker(hand: Vector[Char]): Int = {
  val jokers = hand.count(_ == 'J')
  val map = hand
    .filter(_ != 'J')
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toVector
  val counts = map.map(_._2)

  (map.size, jokers) match {
    case (0, 5) => 6
    case (1, _) => 6
    case (2, j) if j > 1 => 5
    case (2, j) if j <= 1 =>
      if (counts.contains(4 - j))
        5
      else
        4
    case (3, j) if j > 0 => 3
    case (3, 0) =>
      if (counts.contains(3))
        3
      else
        2
    case (4, _) => 1
    case (5, 0) => 0
  }
}
