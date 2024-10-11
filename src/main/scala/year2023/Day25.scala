package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.util.Random

def day25(): Unit = {
  val source = Source.fromFile("resources/2023/day-25")
  val lines = source.getLines().toVector
  source.close()

  val graph = linesToGraph(lines)
  val part1 = findWires(graph)
  println(s"Part 1: $part1")
}

private def findWires(graph: Map[String, Vector[String]]) = {
  var sizes = 0
  var wires = 4
  while (wires > 3) {
    val contractedGraph = contractGraph(mutable.Map(graph.toSeq*))
    val groups = contractedGraph.keys.toVector
    wires = contractedGraph(groups(0)).size

    val size1 = groups(0).count(_ == ':') + 1
    val size2 = groups(1).count(_ == ':') + 1
    sizes = size1 * size2
  }

  sizes
}

@tailrec
private def contractGraph(
    graph: mutable.Map[String, Vector[String]]
): mutable.Map[String, Vector[String]] = {
  if (graph.keys.size <= 2) graph
  else
    val edges = graph.toVector
      .flatMap { (n1, edges) => edges.map { n2 => (n1, n2) } }
    val (n1, n2) = edges(Random.nextInt(edges.size))

    val n1edges = graph(n1).filterNot(_ == n2)
    val n2edges = graph(n2).filterNot(_ == n1)
    val newEdges = n1edges ++ n2edges
    val newNode = s"$n1:$n2"

    graph.remove(n1)
    graph.remove(n2)
    graph(newNode) = newEdges

    val oldNodes = Vector(n1, n2)
    newEdges.foreach { n =>
      oldNodes.foreach { nOld =>
        val i = graph(n).indexOf(nOld)
        if (i >= 0)
          graph(n) = graph(n).updated(i, newNode)
      }
    }

    contractGraph(graph)
}

private def linesToGraph(lines: Vector[String]): Map[String, Vector[String]] = {
  val graph = mutable.Map.empty[String, Vector[String]]
  lines.foreach { case s"$label: $listOfLabels" =>
    val connectedTo = listOfLabels.split(" ").toVector

    if (graph.contains(label))
      graph(label) ++= connectedTo
    else
      graph(label) = connectedTo

    connectedTo.foreach { c =>
      if (graph.contains(c))
        graph(c) ++= Vector(label)
      else
        graph(c) = Vector(label)
    }
  }

  graph.view.mapValues(_.distinct).toMap
}
