package year2022

import scala.collection.mutable.{Stack, Map => MutableMap}
import scala.io.Source

type Index = (Int, Int)

def day12: Unit = {
  val source = Source.fromFile("resources/input-day-12")
  val mapRaw = source.getLines.toArray.map(_.toCharArray.map(_.toInt - 96))
  source.close

  val startInitial = mapRaw.getIndices(-13)(0)
  val end = mapRaw.getIndices(-27)(0)

  val map = mapRaw.map(_.map { value =>
    value match {
      case -13 => 1
      case -27 => 26
      case x   => x
    }
  })

  println(fewestSteps(startInitial, end, map))

  map.getIndices(1).map { ind =>
    val steps = fewestSteps(ind, end, map)
    if (steps < 100000) println(s"$ind $steps")
    steps
  }

  println(map.getIndices(1).map { ind => fewestSteps(ind, end, map) }.min)
}

private def fewestSteps(
    start: Index,
    end: Index,
    map: Array[Array[Int]]
): Int = {
  val visited = MutableMap[Index, (Index, Int)]()
  val candidates = Stack[(Index, Int)]((start, -1))
  var previous = (-1, -1)

  while (candidates.nonEmpty && !visited.contains(end)) {
    val (current, score) = candidates.pop
    visited += (current -> (previous, score + 1))
    candidates.addAll(
      map
        .search(current)
        .filter { c =>
          !visited.contains(c) && !candidates.map(_._1).contains(c)
        }
        .map((_, score + 1))
    )
    previous = current
  }

  if (visited.contains(end))
    visited(end)._2
  else
    100000
}

extension (array: Array[Array[Int]])
  def getIndices(x: Int): Array[Index] = for (
    (row, i) <- array.zipWithIndex;
    (cell, j) <- row.zipWithIndex if cell == x
  ) yield (i, j)

  def get(ind: Index): Int = array(ind._1)(ind._2)

  def search(ind: Index): List[Index] = {
    val directions = List[Index](
      (ind._1 - 1, ind._2),
      (ind._1 + 1, ind._2),
      (ind._1, ind._2 + 1),
      (ind._1, ind._2 - 1)
    )
    val value = array.get(ind)
    val list = for (
      (i, j) <- directions
      if i >= 0 && i < array.size
        && j >= 0 && j < array(0).size
        && array(i)(j) <= value + 1
    ) yield (i, j)
    list
  }
