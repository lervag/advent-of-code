import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

private def day05: Unit = {
  def parse_input(filename: String): (
    List[Stack[Char]],
    List[(Int, Int, Int)]
  ) = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    source.close

    val (stack_lines_raw, procedure_lines) = lines.splitAt(lines.indexOf(""))

    // Number of stacks
    val n = stack_lines_raw.last.split(" ").last.toInt

    // Build the stacks
    val stack_lines = stack_lines_raw.reverse.tail
    val stacks = List range(0, n) map(1 + _*4) map { pos =>
      val stack = Stack[Char]()
      for (line <- stack_lines) {
        if (line.length >= pos) {
          val char = line.charAt(pos)
          if (char != ' ') stack.push(char)
        }
      }
      stack
    }

    val steps = procedure_lines.tail.map(_.split(" ") match {
      case Array(a, b, c, d, e, f) => (b.toInt, d.toInt, f.toInt)
      case _ => (0, 0, 0)
    })

    (stacks, steps)
  }

  val (stacks, steps) = parse_input("resources/input-day-05")

  // for ((move, from, to) <- steps) {
  //   for (_ <- 1 to move) {
  //     stacks(to-1).push(stacks(from-1).pop)
  //   }
  // }
  for ((move, from, to) <- steps) {
    val tmpstack = Stack[Char]()
    for (_ <- 1 to move) {
      tmpstack.push(stacks(from-1).pop)
    }
    stacks(to-1).pushAll(tmpstack)
  }

  val string = (for {
    x <- stacks
  } yield(x.top)).mkString ("")
  println(string)
}
