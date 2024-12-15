package year2024

import cats.parse.{Parser, Rfc5234}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source

def day15: Unit = {
  val source = Source.fromFile("resources/2024/day-15b")
  val input = source.getLines().mkString("\n")
  source.close()

  val p_map = (Parser.charIn('#', '.', 'O', '@').rep <* Rfc5234.lf).rep
    .map { lists =>
      val ny = lists.size
      val nx = lists.head.size
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
      val boxes: Set[Point] = coordinates('O').toSet
      val robot = coordinates('@').head
      (walls, boxes, robot)
    }
  val p_moves = Parser
    .charIn('^', '>', 'v', '<')
    .map {
      case '^' => 0
      case '>' => 1
      case 'v' => 2
      case '<' => 3
    }
    .rep
    .repSep(Rfc5234.lf)
    .map { x =>
      x.toList.toVector.flatMap(
        _.toList.toVector
          .map(Vector[Point]((0, -1), (1, 0), (0, 1), (-1, 0))(_))
      )
    }
  val parser = p_map ~ (Rfc5234.lf.rep0 *> p_moves)

  def move1(
      s: Point,
      dir: Point,
      walls: Set[Point],
      movedBoxes: Set[Point],
      otherBoxes: Set[Point]
  ): Option[(Set[Point], Set[Point])] = {
    val next = s + dir
    if walls.contains(next) then None
    else if otherBoxes.contains(next) then
      move1(next, dir, walls, movedBoxes + (next + dir), otherBoxes - next)
    else Some(movedBoxes, otherBoxes)
  }

  def performMoves1(
      walls: Set[Point],
      boxesInitial: Set[Point],
      robotInitial: Point,
      moves: Vector[Point]
  ) = {
    val (movedBoxes, movedRobot) =
      moves.foldLeft((boxesInitial, robotInitial)) {
        case ((boxes, robot), dir) =>
          move1(robot, dir, walls, Set[Point](), boxes) match {
            case None => (boxes, robot)
            case Some(b1, b2) => (b1 ++ b2, robot + dir)
          }
      }

    movedBoxes.map { (i, j) => i + 100 * j }.sum
  }

  def writeMapToFile(
      n: Int,
      walls: Set[Point],
      boxes: Set[Point],
      robot: Point,
      dir: Point = (0, 0)
  ) = {
    val nx = walls.map(_._1).max
    val ny = walls.map(_._2).max
    val mapAsString = (0 to ny)
      .map { j =>
        (0 to nx).map { i =>
          if robot == (i, j) then
            dir match {
              case (0, 0) => '◉'
              case (1, 0) => "🡆"
              case (-1, 0) => "🡄"
              case (0, 1) => "🡇"
              case (0, -1) => "🡅"
            }
          else if walls.contains((i, j)) then '▒'
          else if boxes.contains((i, j)) then ''
          else if boxes.contains((i - 1, j)) then ' '
          else ' '
        }.mkString
      }
      .mkString("\n")
    Files.write(
      Paths.get(f"map-at-$n%05d.txt"),
      (f"Map: $n%05d\n" + mapAsString + "\n").getBytes(StandardCharsets.UTF_8)
    )
  }

  def move2(
      s: Set[Point],
      dir: Point,
      walls: Set[Point],
      movedBoxes: Set[Point],
      otherBoxes: Set[Point]
  ): Option[(Set[Point], Set[Point])] = {
    val checkPoints = s.map(_ + dir)

    if checkPoints.exists(walls.contains(_)) then None
    else
      val boxesToMove = otherBoxes.filter { b =>
        dir match {
          case (-1, 0) => checkPoints.contains(b + (1, 0))
          case (0, j) if j != 0 =>
            checkPoints.contains(b) || checkPoints.contains(b + (1, 0))
          case _ => checkPoints.contains(b)
        }
      }
      if boxesToMove.size > 0 then
        val nextPositionsToCheck =
          if dir == (1, 0) then boxesToMove.map(_ + dir)
          else if dir == (-1, 0) then boxesToMove
          else boxesToMove.flatMap(x => Set(x, x + (1, 0)))
        move2(
          nextPositionsToCheck,
          dir,
          walls,
          movedBoxes ++ boxesToMove.map(_ + dir),
          otherBoxes -- boxesToMove
        )
      else Some(movedBoxes, otherBoxes)
  }

  def performMoves2(
      walls: Set[Point],
      boxesInitial: Set[Point],
      robotInitial: Point,
      moves: Vector[Point]
  ) = {
    val (movedBoxes, movedRobot) =
      moves.zipWithIndex.foldLeft((boxesInitial, robotInitial)) {
        case ((boxes, robot), (dir, i)) =>
          // writeMapToFile(i, walls, boxes, robot, dir)
          val (b, r) =
            move2(Set(robot), dir, walls, Set[Point](), boxes) match {
              case None => (boxes, robot)
              case Some(b1, b2) => (b1 ++ b2, robot + dir)
            }
          (b, r)
      }
    // writeMapToFile(moves.size, walls, movedBoxes, movedRobot)

    movedBoxes.map { (i, j) => i + 100 * j }.sum
  }

  val part1 = parser
    .parseAll(input)
    .map { case ((w, b, r), m) => performMoves1(w, b, r, m) }
    .getOrElse(-1)

  val part2 = parser
    .parseAll(input)
    .map { case ((w, b, r), m) =>
      val w2 = w.flatMap { (wi, wj) => Set((2 * wi, wj), (2 * wi + 1, wj)) }
      val b2 = b.map { (wi, wj) => (2 * wi, wj) }
      val r2 = (2 * r._1, r._2)
      performMoves2(w2, b2, r2, m)
    }
    .getOrElse(-1)

  println(part1)
  println(part2)
}
