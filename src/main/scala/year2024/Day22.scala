package year2024

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.Queue

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

  def addToBananaScore(
      secret: Long,
      totalBananaScore: mutable.Map[List[Int], Int],
      generations: Int = 2000
  ) = {
    val sequence = Queue[Int]()
    val sequences = mutable.Set[List[Int]]()

    var i = 0
    var s0 = secret
    var s0digit = (s0 % 10).toInt
    while (i < generations) {
      i += 1
      val s1 = evolve(s0)
      val s1digit = (s1 % 10).toInt

      sequence += s1digit - s0digit
      if sequence.size > 4 then sequence.dequeue()

      val seqList = sequence.toList
      if seqList.size == 4 && !sequences.contains(seqList)
      then
        sequences += seqList
        totalBananaScore(seqList) += s1digit

      s0 = s1
      s0digit = s1digit
    }
  }

  val part1 = input.map { secret =>
    (0 until 2000).foldLeft(secret) { (nextSecret, _) => evolve(nextSecret) }
  }.sum
  println(part1)

  val bananaScores = mutable
    .Map[List[Int], Int]()
    .withDefaultValue(0)
  input.foreach { apeSecret =>
    addToBananaScore(apeSecret, bananaScores)
  }

  val part2 = bananaScores.values.max
  println(part2)
}
