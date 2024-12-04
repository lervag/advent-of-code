package year2024

import scala.io.Source
import cats.parse.{Parser => P, Numbers}

def day04: Unit = {
  val source = Source.fromFile("resources/2024/day-04a")
  val lines = source.getLines().toVector
  source.close()

  val strings = {
    val transposed = lines.transpose.map(_.mkString)
    val diagonals1 = (for {
      start <- 1 until lines.length - 3
      diagonal = (start until lines.length)
        .map(i => lines(i)(i - start))
        .mkString
    } yield diagonal).toVector.reverse
    val diagonals2 = (for {
      start <- 0 until lines.length - 3
      diagonal = (start until lines.length)
        .map(i => lines(i - start)(i))
        .mkString
    } yield diagonal).toVector
    val diagonals3 = (for {
      start <- 3 until lines.length
      diagonal = (0 to start)
        .map(i => lines(start - i)(i))
        .mkString
    } yield diagonal).toVector
    val diagonals4 = (for {
      start <- 3 until lines.length - 1
      diagonal = (0 to start)
        .map(i => lines(lines.length - i - 1)(lines.length - start + i - 1))
        .mkString
    } yield diagonal).toVector.reverse
    val all =
      lines ++ transposed ++ diagonals1 ++ diagonals2 ++ diagonals3 ++ diagonals4
    all ++ all.map(_.reverse)
  }

  val parser = (
    P.string("XMAS").map(_ => 1).backtrack
      | P.anyChar.map(_ => 0)
  ).rep.map(_.foldLeft(0)(_ + _))
  val counts = strings.map(parser.parseAll(_).getOrElse(0)).sum
  println(counts)
}
