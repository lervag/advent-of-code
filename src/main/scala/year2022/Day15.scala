import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

private def day15: Unit = {
  case class Beacon(x: Int, y: Int)

  case class Sensor(x: Int, y: Int, range: Int, closest_beacon: Beacon) {
    def coverage(row: Int): Range = {
      val r = range - math.abs(y - row)
      x - r to x + r
    }
  }

  def parseSensor(sx: Int, sy: Int, bx: Int, by: Int)
    = Sensor(sx, sy, math.abs(sx - bx) + math.abs(sy - by), Beacon(bx, by))

  def coverage(sensors: List[Sensor], row: Int) = sensors
    .filter { s => math.abs(s.y - row) < s.range }
    .map { s => s.coverage(row) }
    // .foldLeft(Vector[Int]()) { (a, b) => a.union(b) }

  def search(sensors: List[Sensor], maxRow: Int) = {
    var x: Long = -1
    var y: Long = -1
    while (y <= maxRow && x < 0) {
      y += 1
      if (y % 1000 == 0) {
        println(f"Searching row $y%7d of ${maxRow}")
      }

      val covered = Array.fill(maxRow + 1)(false)
      coverage(sensors, y.toInt) foreach { r =>
        java.util.Arrays.fill(
          covered,
          math.max(0, r.start),
          math.min(maxRow, r.end) + 1,
          true
        )
      }

      x = covered.indexOf(false)
    }

    if (x > 0) {
      println(s"Found ($x, $y)!")
      println(s"Answer is: ${4000000*x + y}")
    }
  }

  val row = 10
  val searchSpace = 20
  val source = Source.fromFile("resources/input-day-15a")
  // val row = 2000000
  // val searchSpace = 4000000
  // val source = Source.fromFile("resources/input-day-15")
  val sensors = source.getLines.toList.map(_ match {
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by"
      => parseSensor(sx.toInt, sy.toInt, bx.toInt, by.toInt)
    case _
      => parseSensor(0, 0, 0, 0)
  })
  source.close

  search(sensors, searchSpace)
}
