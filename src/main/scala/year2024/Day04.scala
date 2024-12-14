package year2024

import cats.parse.Parser as P

import scala.io.Source

def day04: Unit = {
  val source = Source.fromFile("resources/2024/day-04a")
  val lines = source.getLines().toVector
  source.close()

  println(part1(lines))
  println(part2(lines))
}

private def part2(lines: Vector[String]): Integer =
  lines
    .map(_.toVector)
    .slide3x3 { (x, y) =>
      if (x == "MAS" || x == "SAM") && (y == "MAS" || y == "SAM") then 1
      else 0
    }
    .sum

extension (vector: Vector[Vector[Char]])
  private def slide3x3[B](f: (String, String) => B): Vector[B] = {
    (for {
      i <- 0 to vector.length - 3
      j <- 0 to vector.length - 3
    } yield {
      val d1 = (0 until 3).map(k => vector(i + k)(j + k)).mkString
      val d2 = (0 until 3).map(k => vector(i + 2 - k)(j + k)).mkString

      f(d1, d2)
    }).toVector
  }

private def part1(lines: Vector[String]): Integer = {
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

  strings.map(parser.parseAll(_).getOrElse(0)).sum
}
