package year2024

import scala.io.Source
import cats.parse.{Parser => P, Numbers}

def day03: Unit = {
  val source = Source.fromFile("resources/2024/day-03")
  val string = source.getLines().mkString
  source.close()

  val p_int = Numbers.digits.map(_.toInt)
  val p_numbers = p_int ~ (P.char(',') *> p_int)
  val p_muls = P.string("mul") *> p_numbers
    .between(P.char('('), P.char(')'))
    .withContext("asdasd")

  val p1 = (
    p_muls.backtrack.map(Some(_))
      | P.anyChar.void.as(None)
  ).rep0
    .map(_.collect { case Some(result) => result })
  val part1 = p1.parseAll(string).getOrElse(List())
  println(part1.map(_ * _).sum)

  val p2 = (
    (P.string("don't()") ~ P.anyChar.repUntil(P.string("do()"))).backtrack
      .as(None)
      | p_muls.backtrack.map(Some(_))
      | P.anyChar.void.as(None)
  ).rep
    .map(_.collect { case Some(result) => result })

  val part2 = p2.parseAll(string).getOrElse(List())
  println(part2.map(_ * _).sum)
}
