package year2024

import scala.io.Source
import scala.collection.mutable
import cats.parse.{Rfc5234, Parser, Numbers}
import year2023.gcd

def day13: Unit = {
  val source = Source.fromFile("resources/2024/day-13a")
  val input = source.getLines().mkString("\n")
  source.close()

  type BPoint = (BigInt, BigInt)
  extension (a: BPoint) def +(b: BPoint): BPoint = (a._1 + b._1, a._2 + b._2)

  case class Machine(a: BPoint, b: BPoint, prize: BPoint) {
    def fewest_buttons(prize_add: BPoint = (0, 0)): BigInt = {
      // n*a1 + m*b1 = p1
      // n*a2 + m*b2 = p2
      //
      // => n = (p1*b2 - p2*b1)/(a1*b2 - a2*b1)
      //    m = (p1 - n*a1)/b1

      val (a1, a2) = a
      val (b1, b2) = b
      val (p1, p2) = prize + prize_add

      val discriminant = a1 * b2 - a2 * b1
      val numerator = p1 * b2 - p2 * b1
      if discriminant == 0 || numerator % discriminant != 0 then 0
      else
        val n = numerator / discriminant
        val m0 = (p1 - n * a1)
        if m0 % b1 != 0 then 0
        else
          val m = m0 / b1
          3 * n + m
    }
  }

  val p_button =
    (Parser.string("Button ") ~ Rfc5234.alpha ~ Parser.string(": "))
      *> ((Parser.string("X+") *> Numbers.digits.map(BigInt(_)) <* Parser
        .string(
          ", "
        ))
        ~ (Parser.string("Y+") *> Numbers.digits.map(BigInt(_))))
      <* Rfc5234.lf
  val p_prize =
    Parser.string("Prize: ")
      *> ((Parser.string("X=") *> Numbers.digits.map(BigInt(_)) <* Parser
        .string(
          ", "
        ))
        ~ (Parser.string("Y=") *> Numbers.digits.map(BigInt(_))))
  val parser = (p_button ~ p_button ~ p_prize)
    .map { case ((a, b), prize) =>
      Machine(a, b, prize)
    }
    .repSep0(Rfc5234.lf.rep0)

  val machines = parser.parseAll(input).getOrElse(List())
  val part1 = machines.map(_.fewest_buttons()).sum
  val part2 = machines
    .map(_.fewest_buttons((BigInt("10000000000000"), BigInt("10000000000000"))))
    .sum
  println(part1)
  println(part2)
}
