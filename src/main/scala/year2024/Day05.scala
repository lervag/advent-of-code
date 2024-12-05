package year2024

import scala.io.Source
import scala.collection.mutable
import cats.parse.{Parser => P, Numbers}

def day05: Unit = {
  val source = Source.fromFile("resources/2024/day-05a")
  val lines = source.getLines().toVector
  source.close()

  val rules = mutable.Map[Int, Set[Int]]().withDefaultValue(Set())
  lines
    .collect { case s"$x|$y" =>
      (x.toInt, y.toInt)
    }
    .foreach { (x, y) =>
      if (rules.contains(x)) {
        rules(x) = rules(x) + y
      } else {
        rules(x) = Set(y)
      }
    }

  val updates = lines
    .collect {
      case x if x.contains(',') => x.split(",").map(_.toInt).toVector
    }

  def verify(update: Vector[Int]): Boolean =
    !(1 until update.length).exists { i =>
      (0 until i).exists { j =>
        rules(update(i)).contains(update(j))
      }
    }

  val part1 = updates
    .filter(verify)
    .map(update => update(update.length / 2))
    .sum
  println(part1)

  val part2 = updates
    .filter(!verify(_))
    .map(_.sortWith { (a, b) => rules(a).contains(b) })
    .map(update => update(update.length / 2))
    .sum
  println(part2)
}
