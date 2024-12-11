package year2024

import scala.io.Source
import scala.collection.mutable

def day11: Unit = {
  val source = Source.fromFile("resources/2024/day-11a")
  val stones =
    source.getLines().next().split(" ").map(i => (BigInt(i), BigInt(1))).toMap
  source.close()

  val cache = mutable.Map[BigInt, Vector[BigInt]]()
  def blink(x: BigInt) =
    cache.getOrElseUpdate(
      x,
      x match {
        case x if x == 0 => Vector(1)
        case x if x.toString().length % 2 == 0 =>
          try {
            val nstring = x.toString()
            val size = nstring.length
            Vector(
              BigInt(nstring.take(size / 2)),
              BigInt(nstring.drop(size / 2))
            )
          } catch {
            case e =>
              println(x)
              Vector(x)
          }
        case x => Vector(2024 * x)
      }
    )

  val part1 = (0 until 25).foldLeft(stones) { (current, _) =>
    val m = mutable.Map[BigInt, BigInt]().withDefaultValue(0)
    current.foreach { (stone, number) =>
      blink(stone)
        .map(x => (x, number))
        .foreach { (x, y) => m(x) = m(x) + y }
    }
    m.toMap
  }

  println(part1.toVector.map(_._2).sum)

  val part2 = (0 until 75).foldLeft(stones) { (current, _) =>
    val m = mutable.Map[BigInt, BigInt]().withDefaultValue(0)
    current.foreach { (stone, number) =>
      blink(stone)
        .map(x => (x, number))
        .foreach { (x, y) => m(x) = m(x) + y }
    }
    m.toMap
  }

  println(part2.toVector.map(_._2).sum)
}
