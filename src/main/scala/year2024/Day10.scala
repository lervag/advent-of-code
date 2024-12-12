package year2024

import scala.io.Source
import scala.collection.mutable

def day10: Unit = {
  case class TopographicMap(heights: Vector[Vector[Int]]) {
    private val size = heights.size

    def get(position: Point): Int = {
      val (i, j) = position
      if i < 0 || j < 0 || i >= size || j >= size then -1
      else heights(j)(i)
    }

    def get_possible_trailheads(): Vector[Point] =
      heights.zipWithIndex.flatMap { (row, j) =>
        row.zipWithIndex.collect {
          case (cell, i) if cell == 0 => (i, j)
        }
      }
  }

  val source = Source.fromFile("resources/2024/day-10a")
  val topoMap = TopographicMap(
    source.getLines().toVector.map { _.toVector.map { _.asDigit } }
  )
  source.close()

  val directions = List((1, 0), (0, -1), (-1, 0), (0, 1))
  val cache = mutable.Map[Point, List[Point]]()
  def findTrailsDFS(height: Int, position: Point): List[Point] =
    if height == 9 then List(position)
    else cache.getOrElseUpdate(position, {
      val trails = directions
        .map { direction =>
          val nextPos = position + direction
          (topoMap.get(nextPos), nextPos)
        }
        .filter(_._1 == height + 1)
      if trails.size == 0 then List()
      else trails.toList.flatMap(findTrailsDFS.tupled(_))
    })

  val trailheads = topoMap.get_possible_trailheads()
  val scores = trailheads.map { pos => findTrailsDFS(0, pos) }
  val part1 = scores.map(_.toSet.size).sum
  val part2 = scores.map(_.size).sum
  println(part1)
  println(part2)
}
