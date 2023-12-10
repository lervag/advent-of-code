package year2023

import scala.io.Source
import scala.annotation.tailrec

def day10: Unit = {
  val source = Source.fromFile("resources/2023/day-10a")
  val lines = source.getLines.toVector
  source.close()

  // lines -> board Vector(Vector(pipes))
  // S -> board find S
  // loop -> board, S find loop
  //  find two pipes from S
  //  follow one of the pipes back to S
  //  store position and n steps
  // loop: Vector(pos)

  // How many steps along the loop does it take to get from the starting
  // position to the point farthest from the starting position?
}
