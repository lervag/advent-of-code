package year2024

import cats.parse.{Numbers, Parser as P}

import scala.io.Source

def day07: Unit = {
  val parser = (
    (Numbers.digits.map(BigInt(_)) <* (P.char(':') ~ P.char(' ')))
      ~ (Numbers.digits.map(_.toInt).repSep(P.char(' ')))
  )
    .repSep(P.char('\n'))
    .map { result =>
      result.toList.toVector.map { (x, ys) =>
        (x, ys.toList.toVector)
      }
    }

  val source = Source.fromFile("resources/2024/day-07a")
  val lines = source.getLines().mkString("\n")
  val input = parser.parseAll(lines) match {
    case Left(value) =>
      println(value)
      Vector()
    case Right(value) => value
  }
  source.close()

  def add(x: BigInt, y: Int) = x + y
  def mult(x: BigInt, y: Int) = x * y
  def concat(x: BigInt, y: Int) = BigInt(x.toString() + y.toString())

  def check_combine(
      expected: BigInt,
      current: BigInt,
      remaining: Vector[Int],
      operators: Vector[(BigInt, Int) => BigInt]
  ): Boolean =
    if (current > expected) then false
    else if remaining.isEmpty then expected == current
    else
      operators.exists { op =>
        check_combine(
          expected,
          op(current, remaining.head),
          remaining.tail,
          operators
        )
      }

  val part1 = input
    .filter { (expected, numbers) =>
      check_combine(expected, numbers.head, numbers.tail, Vector(add, mult))
    }
    .map((x, _) => x)
    .sum
  println(part1)

  val part2 = input
    .filter { (expected, numbers) =>
      check_combine(
        expected,
        numbers.head,
        numbers.tail,
        Vector(add, mult, concat)
      )
    }
    .map((x, _) => x)
    .sum
  println(part2)
}
