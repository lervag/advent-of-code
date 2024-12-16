package year2024

import cats.parse.{Parser, Rfc5234}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source

def day16: Unit = {
  val testInputs = List(
    ("resources/2024/day-16", 7036),
    ("resources/2024/day-16a", 11048)
  )
  val test = testInputs.head

  
  val source = Source.fromFile(test._1)
  val input = source.getLines().mkString("\n")
  source.close()

  val parser = (Parser.charIn('#', '.', 'E', 'S').rep <* Rfc5234.lf).rep
    .map { lists =>
      val coordinates = lists.toList.zipWithIndex
        .flatMap { (row, j) =>
          row.zipWithIndex.collect {
            case (char, i) if char != '.' => (char, (i, j))
          }
        }
        .groupMap(_._1) { case (_, point: Point) =>
          point
        }
      val walls = coordinates('#').toSet
      val startpos = coordinates('S').head
      val endpos = coordinates('E').head
      (walls, startpos, endpos)
    } <* Rfc5234.lf

  val part1 = parser.parse(input)

  def findShortestPath(
      pos: Point,
      dir: Int,
      points: Int,
      target: Point,
      walls: Set[Point]
  ): Option[Int] = {
    println((pos, dir))
    if walls.contains(pos) then None
    else if pos == target then Some(points)
    else
      val newPos = pos + List((1, 0), (0, -1), (-1, 0), (0, 1))(dir)
      val dirLeft =  dir + 1 % 4
      val dirRight =  dir - 1 % 4
      LazyList(
        findShortestPath(newPos, dir, points + 1, target, walls),
        findShortestPath(pos, dirLeft, points + 1000, target, walls),
        findShortestPath(pos, dirRight, points + 1000, target, walls)
      ).filter(_.nonEmpty)
      .min
  }

  // val part1 = parser
  //   .parse(input)
    // .map { case (w, s, e) => findShortestPath(s, 0, 0, e, w) }
    // .getOrElse(None)

  println(part1)
  // println(part2)
}
