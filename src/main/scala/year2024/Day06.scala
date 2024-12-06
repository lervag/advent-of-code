package year2024

import scala.io.Source
import scala.collection.mutable
import cats.parse.{Parser => P, Numbers}

def day06: Unit = {
  val source = Source.fromFile("resources/2024/day-06a")
  val lines = source.getLines().toVector
  source.close()

  val mapSize = lines.length

  case class Position(i: Int, j: Int) {
    def outOfMap = {
      i > mapSize - 1 || j > mapSize - 1 || i < 0 || j < 0
    }
  }

  val obstacles = lines.zipWithIndex.flatMap { (line, i) =>
    line.zipWithIndex.collect { case ('#', j) => Position(i, j) }
  }.toSet
  val visited = mutable.Set[Position]()

  var p = lines.zipWithIndex.flatMap { (line, i) =>
    line.zipWithIndex.collect { case ('^', j) => Position(i, j) }
  }.head
  var direction = "up"

  var iters = 0

  while (!p.outOfMap && iters < 100000) do {
    iters += 1
    visited += p

    val ((pc, cdir), (pa, adir)) = direction match {
      case "up" =>
        ((Position(p.i - 1, p.j), "up"), (Position(p.i, p.j + 1), "right"))
      case "right" =>
        ((Position(p.i, p.j + 1), "right"), (Position(p.i + 1, p.j), "down"))
      case "down" =>
        ((Position(p.i + 1, p.j), "down"), (Position(p.i, p.j - 1), "left"))
      case "left" =>
        ((Position(p.i, p.j - 1), "left"), (Position(p.i - 1, p.j), "up"))
    }

    if obstacles.contains(pc) then
      p = pa
      direction = adir
    else
      p = pc
      direction = cdir
  }

  println(visited.size)
}
