package year2024

import scala.io.Source
import scala.collection.mutable

def day10: Unit = {
  case class Map(heights: Vector[Vector[Int]]) {
    private val size = heights.size

    def get(i: Int, j: Int): Int = {
      if i < 0 || j < 0 || i >= size || j >= size then -1
      else heights(j)(i)
    }

    def get_possible_trailheads() = heights.zipWithIndex.flatMap { (row, j) =>
      row.zipWithIndex.collect {
        case (cell, i) if cell == 0 => (i, j)
      }
    }
  }

  val source = Source.fromFile("resources/2024/day-10a")
  val map = Map(source.getLines().toVector.map { _.toVector.map { _.asDigit } })
  source.close()

  val trailheads = map.get_possible_trailheads()
  val directions = List((1, 0), (0, -1), (-1, 0), (0, 1))
  val cache = mutable.Map[(Int, Int), Set[(Int, Int)]]()
  def reaches(height: Int, i: Int, j: Int): Set[(Int, Int)] = {
    if cache.contains((i, j)) then cache((i, j))
    else if height == 9 then Set((i, j))
    else
      val trails = directions
        .map { (dx, dy) =>
          val i1 = i + dx
          val j1 = j + dy
          (map.get(i1, j1), i1, j1)
        }
        .filter(_._1 == height + 1)
      if trails.size == 0 then Set()
      else
        cache((i, j)) = trails.toSet.flatMap(reaches.tupled(_))
        cache((i, j))
  }

  val scores = trailheads.map { (i, j) => reaches(0, i, j).size }
  println(scores.sum)
}
