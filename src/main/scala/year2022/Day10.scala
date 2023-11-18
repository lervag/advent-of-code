import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

private def day10: Unit = {
  val source = Source.fromFile("resources/input-day-10")
  val instructions = source.getLines.map{ line =>
    line.split(" ") match {
      case Array("addx", value: String) => (2, value.toInt)
      case Array("noop") => (1, 0)
      case _ => (0, 0)
    }
  }.toArray
  source.close

  var cycle = 1
  var sprite = 1
  val result = for (
    (cycles, add) <- instructions;
    i <- (1 to cycles).reverse
  ) yield {
    cycle += 1
    val pos = (cycle - 2) % 40
    val lit = if (math.abs(sprite - pos) <= 1) '#' else '.'
    if (i == 1) sprite += add
    (cycle, pos, sprite, lit, cycle*sprite)
  }

  val x = result.filter { tuple => (tuple._1 + 20) % 40 == 0 }
  x foreach println
  println(x.map(_._5).sum)

  result.map(_._4).grouped(40).map(_.mkString) foreach println
}
