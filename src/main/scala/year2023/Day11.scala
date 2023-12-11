package year2023

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

def day11: Unit = {
  val source = Source.fromFile("resources/2023/day-11")
  val board = source.getLines.toVector.map(_.toVector)
  source.close()

}
