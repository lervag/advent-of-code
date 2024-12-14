package year2024

import cats.parse.{Numbers, Parser, Rfc5234}
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.io.Source

def day14: Unit = {
  val nx = 101
  val ny = 103
  val filename = "resources/2024/day-14a"
  // val nx = 11
  // val ny = 7
  // val filename = "resources/2024/day-14"

  extension (a: Point)
    def +(b: Point): Point = {
      val x = (a._1 + b._1) % nx
      val y = (a._2 + b._2) % ny
      (if x < 0 then x + nx else x, if y < 0 then y + ny else y)
    }
    def plusTimes(b: Point, n: Int): Point = {
      val x = (a._1 + n * b._1) % nx
      val y = (a._2 + n * b._2) % ny
      (if x < 0 then x + nx else x, if y < 0 then y + ny else y)
    }

  case class Robot(p: Point, v: Point) {
    def move(): Robot = this.copy(p = p + v)
    def moveTimes(n: Int): Robot = this.copy(p = p.plusTimes(v, 100))
  }

  def robotsOnMap(robots: List[Robot]): String =
    (0 until ny)
      .map { j =>
        (0 until nx).map { i =>
          val n = robots.count(_.p == (i, j))
          if n == 1 then '•'
          else if n >= 2 then '●'
          else " "
        }.mkString
      }
      .mkString("\n")

  def writeRobotsOnMap(robots: List[Robot], time: Int): Unit =
    Files.write(
      Paths.get(f"map-$time%05d.txt"),
      (robotsOnMap(robots) + "\n").getBytes(StandardCharsets.UTF_8)
    )

  def robotsInRegion(
      robots: List[Robot],
      i0: Int,
      i1: Int,
      j0: Int,
      j1: Int
  ): Int =
    (for {
      j <- (j0 until j1)
      i <- (i0 until i1)
      n = robots.count(_.p == (i, j))
    } yield n).sum

  def countQuadrants(robots: List[Robot]): Vector[Int] = Vector(
    robotsInRegion(robots, 0, nx / 2, 0, ny / 2),
    robotsInRegion(robots, 0, nx / 2, ny / 2 + 1, ny),
    robotsInRegion(robots, nx / 2 + 1, nx, 0, ny / 2),
    robotsInRegion(robots, nx / 2 + 1, nx, ny / 2 + 1, ny)
  )

  val p_point = (c: Char) =>
    (Parser.string(s"$c=")
      *> Numbers.signedIntString
        .map(_.toInt)
        .repSep(2, 2, Parser.char(','))
        .map { points => (points.head, points.last) })
  val parser = (p_point('p') ~ (Parser.char(' ') *> p_point('v')))
    .map { case (p, v) => Robot(p, v) }
    .repSep0(Rfc5234.lf.rep0)

  val source = Source.fromFile(filename)
  val input = source.getLines().mkString("\n")
  source.close()

  val robots = parser.parseAll(input).getOrElse(List())

  val robotsAt100 = robots.map(_.moveTimes(100))
  val part1 = countQuadrants(robotsAt100).reduce(_ * _)
  println(part1)

  // Fant denne ved å lage massevis av kart og se igjennom
  val part2 = 6355
  println(part2)

  // (1 to 7000).foldLeft(robots) { (r, t) =>
  //   val robots = r.map(_.move())
  //   writeRobotsOnMap(robots, t)
  //   robots
  // }
}
