package year2023

import scala.io.Source

def day03: Unit = {
  val source = Source.fromFile("resources/2023/day-03a")
  val lines = source.getLines.toVector
  source.close()

  // println(s"Sum of game ids: ${filteredGames.map(_._1).sum}")
}
