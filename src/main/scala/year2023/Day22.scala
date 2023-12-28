package year2023

import scala.io.Source
import scala.collection.mutable

def day22(): Unit = {
  val source = Source.fromFile("resources/2023/day-22")
  val lines = source.getLines().toVector
  source.close()

  val bricks = parseLines(lines)
  val brickMap = bricks.map { b => b.id -> b }.toMap

  relax(bricks)
  inspectInPlace(bricks, brickMap)

  val part1 = bricks.filter { b =>
    b.supports.forall { id => brickMap(id).supportedBy.size > 1 }
  }.size
  println(s"Part 1: $part1")

  val part2 = findSumOfBricksToFall(bricks, brickMap)
  println(s"Part 2: $part2")
}

private def findSumOfBricksToFall(
    bricks: Vector[Brick],
    brickMap: Map[Int, Brick]
) =
  bricks
    .filterNot { b =>
      b.supports.forall { id => brickMap(id).supportedBy.size > 1 }
    }
    .sortBy(_.bottom)
    .reverse
    .map { b => dropCountAffected(b, brickMap) }
    .sum

private def dropCountAffected(b: Brick, brickMapIn: Map[Int, Brick]): Int = {
  // Use a copy of the original brickmap because we alter state of bricks
  val brickMap = brickMapIn.map { (key, value) =>
    val v = value.copy()
    v.supportedBy ++= value.supportedBy
    v.supports ++= value.supports
    key -> v
  }

  val dropped = dropSupports(b, brickMap, mutable.Set(b.id))
  dropped.size - 1
}

private def dropSupports(b: Brick, brickMap: Map[Int, Brick], dropped: mutable.Set[Int]): mutable.Set[Int] = {
  b.supports.foreach { id =>
    val bb = brickMap(id)
    bb.supportedBy -= b.id
    if (bb.supportedBy.size == 0)
      dropped += id
      dropped ++= dropSupports(bb, brickMap, dropped)
  }

  dropped
}

private def relax(bricks: Vector[Brick]): Unit = {
  val brickHeightMap = bricks.groupBy(_.bottom)
  val height = bricks.maxBy(_.top).top
  val width = bricks.maxBy(_.depth).depth
  var snitt = mutable.ArrayBuffer.fill(width + 1, width + 1)((0, 0))

  (1 to height) foreach { k =>
    brickHeightMap
      .get(k)
      .map(_.foreach { b =>
        val below = (for {
          i <- (b.p1.x to b.p2.x)
          j <- (b.p1.y to b.p2.y)
        } yield snitt(i)(j)).maxBy(_._2)

        val distance = b.bottom - below._2

        if (distance > 1)
          b.fall(distance - 1)

        for {
          i <- (b.p1.x to b.p2.x)
          j <- (b.p1.y to b.p2.y)
        } {
          snitt(i)(j) = (b.id, b.top)
        }
      })
  }
}

private def parseLines(lines: Vector[String]): Vector[Brick] =
  lines.zipWithIndex
    .map { (line, id) =>
      line match {
        case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
          Brick(
            id + 1,
            Coordinate(x1.toInt, y1.toInt, z1.toInt),
            Coordinate(x2.toInt, y2.toInt, z2.toInt)
          )
      }
    }
    .sortBy(_.bottom)

private def inspectInPlace(
    bricks: Vector[Brick],
    brickMap: Map[Int, Brick]
): Unit = {
  val brickHeightMap = bricks.groupBy(_.bottom)
  val height = bricks.maxBy(_.top).top
  val width = bricks.maxBy(_.depth).depth
  var snitt = mutable.ArrayBuffer.fill(width + 1, width + 1)(0)

  (1 to height) foreach { k =>
    brickHeightMap
      .get(k)
      .map(_.foreach { b =>
        val belowIds = (for {
          i <- (b.p1.x to b.p2.x)
          j <- (b.p1.y to b.p2.y)
        } yield snitt(i)(j))
          .filter { id =>
            id > 0 && brickMap(id).top == b.bottom - 1
          }

        b.supportedBy ++= belowIds

        b.supportedBy.foreach { id => brickMap(id).supports += b.id }

        for {
          i <- (b.p1.x to b.p2.x)
          j <- (b.p1.y to b.p2.y)
        } {
          snitt(i)(j) = b.id
        }
      })
  }
}

case class Coordinate(x: Int, y: Int, var z: Int) {
  override def toString(): String =
    s"($x $y $z)"
}

case class Brick(id: Int, p1: Coordinate, p2: Coordinate) {
  val supports = mutable.Set.empty[Int]
  val supportedBy = mutable.Set.empty[Int]

  def top: Int = p1.z.max(p2.z)
  def bottom: Int = p1.z.min(p2.z)
  def depth: Int = p1.x.max(p2.x)

  def fall(distance: Int) = {
    p1.z -= distance
    p2.z -= distance
  }

  override def toString(): String =
    s"Brick $id $p1, $p2\n  Supported by: $supportedBy\n  Supports:     $supports"
}
