package year2024

import scala.io.Source
import scala.collection.mutable
import cats.parse.{Parser => P, Numbers}

def day09: Unit = {
  val source = Source.fromFile("resources/2024/day-09a")
  val input = source.getLines().next()
  source.close()

  val p_int = Numbers.digit.map(_.asDigit)
  val p = (p_int ~ (p_int ~ p_int).rep0(0)).map { case (h, t) =>
    (0, h) +: t.toVector
  }
  val parsed = p
    .parseAll(input)
    .getOrElse(Vector())
    .zipWithIndex
    .map { case ((x, y), z) => (x, (z, y)) }

  val n = parsed.size
  val (_, (_, size)) = parsed.last
  val part1 = parsed
    .foldLeft((n - 1, size, Vector[Int]())) {
      case ((p0, r0, acc), (free, (id, size))) if p0 < id =>
        (0, 0, acc)
      case ((p0, r0, acc), (free, (id, size))) if p0 == id =>
        val collected = (1 to r0).map(_ => id).toVector
        (0, 0, acc ++ collected)
      case ((p0, r0, acc), (free, (id, size))) =>
        var p = p0
        var r = r0
        val moved = (1 to free).map { _ =>
          val id1 = {
            if r == 0 then
              p -= 1
              val (_, (idd, f)) = parsed(p)
              r = f - 1
              idd
            else
              val (_, (idd, _)) = parsed(p)
              r -= 1
              idd
          }

          id1
        }.toVector
        val collected = (0 until size).map(_ => id).toVector
        (p, r, acc ++ moved ++ collected)
    }
    ._3
    .zipWithIndex
    .foldLeft(BigInt(0)) { case (checksum, (id, number)) =>
      checksum + id * number
    }
  println(part1)
}
