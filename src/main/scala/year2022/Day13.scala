package year2022

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import ujson._

def day13: Unit = {
  val source = Source.fromFile("resources/input-day-13")
  val pairs = source.getLines
    .filter(!_.isEmpty)
    .map(read(_))
    .toList
  source.close

  val countRight = pairs
    .grouped(2)
    .map { seq => compare(seq(0).arr.clone, seq(1).arr.clone) }
    .zipWithIndex
    .filter { (string, index) => string == "right" }
    .map(_._2 + 1)
    .sum

  println(countRight)

  val divider1 = read("[[2]]")
  val divider2 = read("[[6]]")
  val pairsExtended = pairs ++ List(divider1, divider2)
  val sortedPairs = pairsExtended.sortWith { (left, right) =>
    compare(left.arr.clone, right.arr.clone) match {
      case "right" => true
      case _       => false
    }
  }

  println(
    (sortedPairs.indexOf(divider1) + 1) * (sortedPairs.indexOf(divider2) + 1)
  )
}

private def compare(x: ArrayBuffer[Value], y: ArrayBuffer[Value]): String = {
  if (x.isEmpty && y.isEmpty) "continue"
  else if (x.isEmpty) "right"
  else if (y.isEmpty) "wrong"
  else
    val state = (x.remove(0), y.remove(0)) match {
      case (a1: Arr, b1: Arr) => compare(a1.arr.clone, b1.arr.clone)
      case (a1: Num, b1: Num) =>
        if (a1.num < b1.num)
          "right"
        else if (a1.num > b1.num)
          "wrong"
        else
          "continue"
      case (a1: Num, b1: Arr) => compare(Arr(a1).arr, b1.arr.clone)
      case (a1: Arr, b1: Num) => compare(a1.arr.clone, Arr(b1).arr)
      case _                  => "continue"
    }

    if (state == "continue")
      compare(x, y)
    else
      state
}
