package year2023

import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source

def day16: Unit = {
  val source = Source.fromFile("resources/2023/day-16")
  val contraption = Contraption(source.getLines().toVector.map(_.toVector))
  source.close()

  val part1 = contraption
    .runLightDFS(Beam(0, 0, Direction.Right))
    .size
  println(s"Part 1: $part1")

  val n = contraption.range.max
  val beams = for {
    i <- contraption.range
    d <- Direction.values
  } yield (
    d match {
      case Direction.Left => Beam(i, n, d)
      case Direction.Up => Beam(n, i, d)
      case Direction.Right => Beam(i, 0, d)
      case Direction.Down => Beam(0, i, d)
    }
  )
  val part2 = beams.map(contraption.runLightDFS(_).size).max
  println(s"Part 2: $part2")
}

private enum Direction:
  case Left, Up, Right, Down

sealed case class Contraption(mirrors: Vector[Vector[Char]]) {
  val range = 0 until mirrors.size

  def runLightDFS(beam: Beam) = {
    val energized = mutable.Map
      .empty[(Int, Int), Boolean]
      .withDefaultValue(false)
    val beamStack = Stack(beam)
    val beamCreated = collection.mutable.Set[(Int, Int)]()
    while (beamStack.nonEmpty) {
      val cur = beamStack.pop
      val beamsVisited = mutable.Set.empty[((Int, Int), Direction)]
      while (
        range.contains(cur.x)
        && range.contains(cur.y)
        && !beamsVisited.contains((cur.position, cur.direction))
      ) {
        energized(cur.position) = true
        beamsVisited += ((cur.position, cur.direction))
        cur.direction = (mirrors(cur.y)(cur.x), cur.direction) match {
          case ('|', Direction.Right | Direction.Left) =>
            if (!beamCreated.contains(cur.position))
              beamStack += cur.split()
              beamCreated += beamStack.last.position
            Direction.Up
          case ('-', Direction.Up | Direction.Down) =>
            if (!beamCreated.contains(cur.position))
              beamStack += cur.split()
              beamCreated += beamStack.last.position
            Direction.Left
          case ('/', Direction.Left) => Direction.Down
          case ('/', Direction.Up) => Direction.Right
          case ('/', Direction.Right) => Direction.Up
          case ('/', Direction.Down) => Direction.Left
          case ('\\', Direction.Left) => Direction.Up
          case ('\\', Direction.Right) => Direction.Down
          case ('\\', Direction.Down) => Direction.Right
          case ('\\', Direction.Up) => Direction.Left
          case _ => cur.direction
        }
        cur.direction match {
          case Direction.Left => cur.x -= 1
          case Direction.Up => cur.y -= 1
          case Direction.Right => cur.x += 1
          case Direction.Down => cur.y += 1
        }
      }
    }
    energized
  }

  override def toString() = {
    mirrors
      .map { row =>
        row.map {
          _ match {
            case '.' => ' '
            case chr => chr
          }
        }.mkString
      }
      .mkString("\n")
  }
}

sealed class Beam(var y: Int, var x: Int, var direction: Direction) {
  def position = (y, x)

  def split() =
    direction match {
      case Direction.Left => Beam(y, x, Direction.Down)
      case Direction.Up => Beam(y, x, Direction.Right)
      case Direction.Right => Beam(y, x, Direction.Down)
      case Direction.Down => Beam(y, x, Direction.Right)
    }

  override def toString() = {
    val d = direction match {
      case Direction.Left => ""
      case Direction.Up => ""
      case Direction.Right => ""
      case Direction.Down => ""
    }
    s"($y, $x) $d "
  }
}

private object Beam {
  def apply(y: Int, x: Int, direction: Direction) = new Beam(y, x, direction)
}
