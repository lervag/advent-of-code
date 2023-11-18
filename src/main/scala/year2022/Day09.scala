package year2022

import scala.collection.mutable.{Map => MutableMap}
import scala.io.Source

def day09: Unit = {
  val source = Source.fromFile("resources/input-day-09")
  val input = source.getLines.toArray
  source.close

  val steps = input
    .map(_.split(" ") match {
      case Array(a, b) => (a, b.toInt)
      case _           => ("", 0)
    })
    .flatMap { (a, b) => List.fill(b)(a) }

  val visited = MutableMap[(Int, Int), Boolean]()
  var posH = (0, 0)
  var pos1 = (0, 0)
  var pos2 = (0, 0)
  var pos3 = (0, 0)
  var pos4 = (0, 0)
  var pos5 = (0, 0)
  var pos6 = (0, 0)
  var pos7 = (0, 0)
  var pos8 = (0, 0)
  var pos9 = (0, 0)
  visited(pos9) = true

  var i = 0
  steps foreach { step =>
    i += 1
    posH = step match {
      case "L" => (posH._1 - 1, posH._2)
      case "D" => (posH._1, posH._2 - 1)
      case "R" => (posH._1 + 1, posH._2)
      case "U" => (posH._1, posH._2 + 1)
      case _   => (0, 0)
    }
    pos1 = moveTail(pos1, posH)
    pos2 = moveTail(pos2, pos1)
    pos3 = moveTail(pos3, pos2)
    pos4 = moveTail(pos4, pos3)
    pos5 = moveTail(pos5, pos4)
    pos6 = moveTail(pos6, pos5)
    pos7 = moveTail(pos7, pos6)
    pos8 = moveTail(pos8, pos7)
    pos9 = moveTail(pos9, pos8)
    visited(pos9) = true
  }

  println(visited.size)
}

private def moveTail(t: (Int, Int), h: (Int, Int)): (Int, Int) = {
  if (t._1 < h._1 - 1)
    if (t._2 < h._2 - 1)
      (h._1 - 1, h._2 - 1)
    else if (t._2 > h._2 + 1)
      (h._1 - 1, h._2 + 1)
    else
      (h._1 - 1, h._2)
  else if (t._1 > h._1 + 1)
    if (t._2 < h._2 - 1)
      (h._1 + 1, h._2 - 1)
    else if (t._2 > h._2 + 1)
      (h._1 + 1, h._2 + 1)
    else
      (h._1 + 1, h._2)
  else if (t._2 < h._2 - 1)
    (h._1, h._2 - 1)
  else if (t._2 > h._2 + 1)
    (h._1, h._2 + 1)
  else
    t
}
