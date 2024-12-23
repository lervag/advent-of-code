package year2024

import scala.collection.mutable
import scala.io.Source

def day23: Unit = {
  val inputFile = List("resources/2024/day-23", "resources/2024/day-23a")(1)
  val source = Source.fromFile(inputFile)
  val connections = source
    .getLines()
    .map(_.split("-"))
    .collect { case Array(from, to) =>
      (from, to)
    }
    .toVector
  source.close()

  val adjacencyMap = pairsToAdjMap(connections)
  val part1 = findTriplets(adjacencyMap)
  println(part1.size)

  val maxSize = 13

  println(part2)
}

private def findTriplets(
    adjacencyMap: Map[String, Vector[String]]
): Set[Vector[String]] =
  adjacencyMap.toVector
    .flatMap { case ((node0, connections)) =>
      connections
        .combinations(2)
        .collect {
          case Vector(node1, node2) if adjacencyMap(node1).contains(node2) =>
            Vector(node0, node1, node2).sorted
        }
    }
    .collect {
      case triplet @ Vector(node0, node1, node2)
          if node0.startsWith("t")
            || node1.startsWith("t")
            || node2.startsWith("t") =>
        triplet
    }
    .toSet

private def pairsToAdjMap[A](list: Seq[(A, A)]): Map[A, Vector[A]] = {
  val amap = mutable.Map[A, Vector[A]]().withDefaultValue(Vector())

  for ((node1, node2) <- list) {
    amap(node1) = amap(node1) :+ node2
    amap(node2) = amap(node2) :+ node1
  }

  amap.toMap
}
