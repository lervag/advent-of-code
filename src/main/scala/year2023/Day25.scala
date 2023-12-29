package year2023

import scala.io.Source
import scala.collection.mutable

def day25(): Unit = {
  val source = Source.fromFile("resources/2023/day-25a")
  val lines = source.getLines().toVector
  source.close()

  val connections = linesToConnections(lines)
  connections.foreach(println)
  // val part1 = ???
  // println(s"Part 1: $part1")

  // val part2 = ???
  // println(s"Part 2: $part2")
}

private def disconnect(
    c1: String,
    c2: String,
    connections: Map[String, Vector[String]]
) = connections
  .updated(c1, connections(c1).filter(_ != c2))
  .updated(c2, connections(c2).filter(_ != c1))

private def linesToConnections(lines: Vector[String]) = {
  val connections = mutable.Map.empty[String, Vector[String]]
  lines.foreach { case s"$label: $listOfLabels" =>
    val connectedTo = listOfLabels.split(" ").toVector

    if (connections.contains(label))
      connections(label) ++= connectedTo
    else
      connections(label) = connectedTo

    connectedTo.foreach { c =>
      if (connections.contains(c))
        connections(c) ++= Vector(label)
      else
        connections(c) = Vector(label)
    }
  }

  connections
    .mapValues(_.distinct)
    .toMap
}
