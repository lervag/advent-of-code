package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

def day03: Unit = {
  val source = Source.fromFile("resources/2023/day-03")
  val lines = source.getLines.toVector
  source.close()

  val reSymbol = "[^.0-9]".r
  val symbols = lines.map { line =>
    line.toVector.map { char =>
      char != '.' && !char.isDigit
    }
  }
  val gears = lines.map { line => line.toVector.map(_ == '*') }
  val gearMap = mutable.Map.empty[(Int, Int), ArrayBuffer[Int]]

  val nx = lines(0).size
  val ny = lines.size
  val reNumber = "\\d+".r

  var sum = 0
  lines.zipWithIndex.foreach { (line, index) =>
    val i0 = math.max(index-1, 0)
    val i1 = math.min(index+1, ny-1)
    val symbolRows = symbols.slice(i0, i1+1)

    reNumber.findAllMatchIn(line).foreach { m =>
      val j0 = math.max(m.start - 1, 0)
      val j1 = math.min(m.end, nx-1)
      val number = m.matched.toInt
      if (symbolRows.exists(_.slice(j0, j1+1).exists( s => s )))
        sum += number

      for {
        i <- i0 to i1
        j <- j0 to j1
        if gears(i)(j)
      } {
        gearMap.getOrElseUpdate((i, j), ArrayBuffer.empty[Int]) += number
      }
    }
  }

  val gearSum = gearMap.filter(_._2.size == 2).values.map(_.foldLeft(1)(_*_)).sum

  println(s"Part 1: $sum")
  println(s"Part 2: $gearSum")
}
