package year2024

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

def day22: Unit = {
  val inputFile = List("resources/2024/day-22", "resources/2024/day-22a")(1)
  val source = Source.fromFile(inputFile)
  val input = source.getLines().map(_.toLong).toVector
  source.close()

  def mixAndPrune(secret: Long, value: Long): Long =
    (value ^ secret) % 16777216

  def evolve(secret: Long): Long = {
    val s1 = mixAndPrune(secret, 64 * secret)
    val s2 = mixAndPrune(s1, s1 / 32)
    mixAndPrune(s2, 2048 * s2)
  }

  // val part1 = input.map { secret =>
  //   (0 until 2000).foldLeft(secret) { (nextSecret, _) => evolve(nextSecret) }
  // }.sum

  def getChanges(secret: Long, generations: Int = 2000) = {
    var s0 = secret
    var s0digit = (s0 % 10).toInt
    val prices = ListBuffer[Int]()
    val changes = ListBuffer[Int]()
    var i = 0
    while (i < generations) {
      i += 1
      val s1 = evolve(s0)
      val s1digit = (s1 % 10).toInt
      prices += s1digit
      changes += s1digit - s0digit
      s0 = s1
      s0digit = s1digit
    }

    (prices.toVector, changes.toVector)
  }

  def getSales(prices: Vector[Int], changes: Vector[Int], sequence: Vector[Int]) =
    changes
      .sliding(4)
      .indexOf(sequence) match {
      case -1 => 0
      case index => prices(index + 3)
    }

  val changes = input
    .map { secret => getChanges(secret) }

  val range = -9 to 9
  val allCombinations = for {
    a <- range
    b <- range
    c <- range
    d <- range
    seq = Vector(a, b, c, d)
    bananas = changes
      .map { (p, c) => getSales(p, c, seq) }
      .sum
    _ = println(s"$seq gives $bananas bananas")
  } yield bananas

  val part2 = allCombinations.max

  println(part1)
  println(part2)
}
