package year2022

import scala.collection.mutable.{Map => MutableMap}
import scala.io.Source

def day14: Unit = {
  val stoneMap = MutableMap[(Int, Int), Char]()
  val origin = (500, 0)
  stoneMap(origin) = '+'

  val source = Source.fromFile("resources/input-day-14a")
  source.getLines foreach { line =>
    line
      .split(" -> ")
      .map { points =>
        val coords = points.split(',').map(_.toInt)
        (coords(0), coords(1))
      }
      .reduce { (c1, c2) =>
        val x = c1._1
        val y = c1._2
        if (c2._1 == x) {
          (if (c1._2 < c2._2)
             (c1._2 to c2._2)
           else
             (c2._2 to c1._2)) foreach { z => stoneMap((x, z)) = '#' }
        } else {
          (if (c1._1 < c2._1)
             (c1._1 to c2._1)
           else
             (c2._1 to c1._1)) foreach { z => stoneMap((z, y)) = '#' }
        }
        c2
      }
  }
  source.close

  // draw(stoneMap)
  while (pourSand(stoneMap, true, origin)) {}
  // draw(stoneMap)

  println(stoneMap.values.count(_ == 'o'))
}

private def draw(stoneMap: MutableMap[(Int, Int), Char]): Unit = {
  val xs = stoneMap.keys.map(_._1)
  val ys = stoneMap.keys.map(_._2)
  val xrange = xs.min to xs.max
  val yrange = ys.min to ys.max
  yrange foreach { y =>
    xrange foreach { x =>
      print(stoneMap.getOrElse((x, y), ' '))
    }
    print("\n")
  }
}

private def pourSand(
    m: MutableMap[(Int, Int), Char],
    floor: Boolean,
    origin: (Int, Int)
): Boolean = {
  var (x, y) = origin
  if (m(origin) == 'o') {
    false
  } else {
    val ymax = m.filter(_._2 == '#').keys.map(_._2).max + 2
    var flowing = true
    while (flowing && y < ymax) {
      y += 1
      if (!m.contains((x, y))) {
        // pass
      } else if (!m.contains((x - 1, y))) {
        x -= 1
      } else if (!m.contains((x + 1, y))) {
        x += 1
      } else {
        flowing = false
        m((x, y - 1)) = 'o'
      }
    }

    if (floor && y == ymax) {
      flowing = false
      m((x, y - 1)) = 'o'
    }

    !flowing
  }
}
