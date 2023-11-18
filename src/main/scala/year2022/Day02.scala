package year2022

import ujson._

import scala.io.Source

def day02: Unit = {
  val source = Source.fromFile("resources/input-day-02")
  val pairs = source.getLines().map(_.split(" ")).toList
  println(s"Sum 1: ${pairs.map(play1).sum}")
  println(s"Sum 2: ${pairs.map(play2).sum}")
  source.close()
}

private def play1(pair: Array[String]): Int = {
  val outcome = pair match {
    case Array("A", "X") => 3
    case Array("A", "Y") => 6
    case Array("A", "Z") => 0
    case Array("B", "X") => 0
    case Array("B", "Y") => 3
    case Array("B", "Z") => 6
    case Array("C", "X") => 6
    case Array("C", "Y") => 0
    case Array("C", "Z") => 3
    case _               => 0
  }
  val shape = pair(1) match {
    case "X" => 1
    case "Y" => 2
    case "Z" => 3
    case _   => 0
  }
  outcome + shape
}

private def play2(pair: Array[String]): Int = pair match {
  case Array("A", "X") => 3
  case Array("A", "Y") => 4
  case Array("A", "Z") => 8
  case Array("B", "X") => 1
  case Array("B", "Y") => 5
  case Array("B", "Z") => 9
  case Array("C", "X") => 2
  case Array("C", "Y") => 6
  case Array("C", "Z") => 7
  case _               => 0
}
