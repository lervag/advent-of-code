package year2024

import scala.io.Source
import scala.collection.mutable
import cats.parse.{Parser => P, Numbers}

case class Position(i: Int, j: Int)

def day06: Unit = {
  val source = Source.fromFile("resources/2024/day-06b")
  val lines = source.getLines().toVector
  source.close()

  val mapSize = lines.length

  val obstacles = lines.zipWithIndex.flatMap { (line, i) =>
    line.zipWithIndex.collect { case ('#', j) => Position(i, j) }
  }.toSet

  val startPosition = lines.zipWithIndex.flatMap { (line, i) =>
    line.zipWithIndex.collect { case ('^', j) => Position(i, j) }
  }.head

  val part1_path = get_path(startPosition, mapSize, obstacles)._3
  val part1 = part1_path.size
  part1_path.tapEach(println)
  println(part1)

  // val part2 = (for {
  //   p <- part1_path - startPosition
  //   newObstacles = obstacles + p
  //   (cycle, _) = get_path(startPosition, mapSize, newObstacles)
  //   if cycle
  // } yield p).tapEach(println).size
  // println(part2)
}

def get_path(
    startPosition: Position,
    size: Int,
    obstacles: Set[Position]
) = {
  var p = startPosition
  var direction = "up"
  var cycle = false
  val visited = mutable.Set[(Position, String)]()
  val v = mutable.ArrayBuffer[Position]()
  while (!cycle && p.i < size && p.j < size && p.i >= 0 && p.j >= 0) do {
    visited += ((p, direction))
    v += p

    // TODO: Gjør ___ 1 ___ ting om gangen!
    val ((pos_next, dir_next), (pos_next_wall, dir_next_wall)) =
      direction match {
        case "up" =>
          ((Position(p.i - 1, p.j), "up"), (Position(p.i, p.j + 1), "right"))
        case "right" =>
          ((Position(p.i, p.j + 1), "right"), (Position(p.i + 1, p.j), "down"))
        case "down" =>
          ((Position(p.i + 1, p.j), "down"), (Position(p.i, p.j - 1), "left"))
        case "left" =>
          ((Position(p.i, p.j - 1), "left"), (Position(p.i - 1, p.j), "up"))
      }

    if obstacles.contains(pos_next) then
      p = pos_next_wall
      direction = dir_next_wall
    else
      p = pos_next
      direction = dir_next

    cycle = visited.contains((p, direction))
  }

  (cycle, visited.map((p, _) => p).toSet, v)
}
