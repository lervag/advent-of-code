package year2023

import scala.io.Source
import scala.collection.mutable

def day13: Unit = {
  val source = Source.fromFile("resources/2023/day-13a")
  val lines = source.getLines.toVector
  source.close()

  // split lines into "chunks"
  // split chunks into patterns
  // parse pattern into tuple of
  //  list of columns (BitMap)
  //  list of rows (BitMap)
  // for each list
  //  for i from 1 to list length - 1
  //    compare numbers in list (i-, i+)
  //      if equal -- tthis is the reflection and i is my number

  val part1 = ??? // number of cols to the left + 100 number of rows above
  println(s"Part 1: $part1")

  val part2 = ???
  println(s"Part 2: $part2")
}
