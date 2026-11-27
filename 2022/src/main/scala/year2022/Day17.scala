package year2022

import scala.io.Source
import scala.collection.mutable

sealed trait JetDirection
case object Left extends JetDirection
case object Right extends JetDirection

case class Rock(shape: Set[(Int, Int)]):
  val width: Int =
    if shape.isEmpty then 0 else shape.map(_._1).max - shape.map(_._1).min + 1

  def placeAt(x: Int, y: Int): Rock =
    this.copy(shape = shape.map((dx, dy) => (x + dx, y + dy)))

  def moveLeft: Rock =
    if shape.map(_._1).min > 0 then
      this.copy(shape = shape.map((x, y) => (x - 1, y)))
    else this

  def moveRight: Rock =
    if shape.map(_._1).max < 6 then
      this.copy(shape = shape.map((x, y) => (x + 1, y)))
    else this

  def move(direction: JetDirection): Rock = direction match
    case Left => moveLeft
    case Right => moveRight

  def fall: Rock =
    this.copy(shape = shape.map((x, y) => (x, y - 1)))

object Rocks:
  val rocks: List[Rock] = List(
    Rock(Set((0, 0), (1, 0), (2, 0), (3, 0))),
    Rock(Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))),
    Rock(Set((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))),
    Rock(Set((0, 0), (0, 1), (0, 2), (0, 3))),
    Rock(Set((0, 0), (1, 0), (0, 1), (1, 1)))
  )

case class Chamber(width: Int = 7):
  private val occupied: mutable.Set[(Int, Int)] = mutable.Set.empty
  private val colTop: Array[Int] = Array.fill(width)(-1)
  private var height: Int = 0

  def highestPoint: Int = height

  def render(shape: Set[(Int, Int)] = Set.empty): String =
    val top = Math.max(highestPoint, shape.map(_._2).maxOption.getOrElse(0))
    val column = (0 to Math.max(5, top)).reverse
      .map { y =>
        val l = (0 until width).map { x =>
          if shape.contains((x, y)) then '@'
          else if occupied.contains((x, y)) then '#'
          else '.'
        }.mkString
        f"|${l}|"
      }
      .mkString("\n")
    f"${column}\n+-------+"

  private def collides(rock: Rock): Boolean =
    rock.shape.exists { case (x, y) => y < 0 || occupied.contains((x, y)) }

  /** Depth of each column below the top, capped so that deep, unreachable
    * holes don't prevent the state from repeating.
    */
  def profile: Vector[Int] =
    colTop.map(t => Math.min(height - 1 - t, 50)).toVector

  /** Drop a rock and return the index of the last jet that was used. */
  def addRock(rock: Rock, jetstream: Iterator[(Int, JetDirection)]): Int =
    @annotation.tailrec
    def loop(r: Rock): (Int, Rock) =
      val (i_j, dir) = jetstream.next()
      val afterJet = r.move(dir)
      val r1 = if collides(afterJet) then r else afterJet

      val afterFall = r1.fall
      if collides(afterFall) then (i_j, r1)
      else loop(afterFall)

    val (i_j, landed) = loop(rock.placeAt(2, height + 3))
    occupied ++= landed.shape
    for (x, y) <- landed.shape do
      colTop(x) = Math.max(colTop(x), y)
      height = Math.max(height, y + 1)
    i_j

def day17: Unit = {
  val input = Source.fromFile("inputs/day-17").getLines.next()
  // val input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

  /** Height of the tower after `target` rocks, using cycle detection to skip
    * ahead.
    */
  def heightAfter(target: Long): Long =
    val chamber = Chamber()
    val jetstream: Iterator[(Int, JetDirection)] =
      Iterator
        .continually(input.zipWithIndex)
        .flatten
        .map {
          case ('>', i) => (i, Right)
          case (_, i)   => (i, Left)
        }
    val rocks = Iterator.continually(Rocks.rocks.zipWithIndex).flatten

    // heights(n) = tower height after n rocks
    val heights = mutable.ArrayBuffer[Int](0)
    val seen = mutable.Map[(Vector[Int], Int, Int), Int]()

    var result: Option[Long] = None
    var n = 0
    while result.isEmpty do
      if n == target then result = Some(chamber.highestPoint.toLong)
      else
        val (rock, i_r) = rocks.next()
        val i_j = chamber.addRock(rock, jetstream)
        n += 1
        heights += chamber.highestPoint

        val key = (chamber.profile, i_r, i_j)
        seen.get(key) match
          case Some(n0) =>
            val cycleLen = n - n0
            val cycleHeight = (heights(n) - heights(n0)).toLong
            val remaining = target - n0
            val fullCycles = remaining / cycleLen
            val rest = (remaining % cycleLen).toInt
            result = Some(heights(n0 + rest) + fullCycles * cycleHeight)
          case None =>
            seen(key) = n

    result.get

  println(f"Part 1: ${heightAfter(2022)}")
  println(f"Part 2: ${heightAfter(1000000000000L)}")
  // example answers: 3068, 1514285714288
}
