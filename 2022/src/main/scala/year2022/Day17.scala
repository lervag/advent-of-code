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
    this.copy(shape = shape.map((x, y) => (x - 1, y)))

  def moveRight: Rock =
    this.copy(shape = shape.map((x, y) => (x + 1, y)))

  def move(direction: JetDirection): Rock = direction match
    case Left => moveLeft
    case Right => moveRight

  def fall: Rock =
    this.copy(shape = shape.map((x, y) => (x, y - 1)))

object Rocks:
  val rocks: List[Rock] = List(
    Rock(Set((0, 0), (1, 0), (2, 0), (3, 0))),
    Rock(Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))),
    Rock(Set((2, 0), (2, 1), (0, 2), (1, 2), (2, 2))),
    Rock(Set((0, 0), (0, 1), (0, 2), (0, 3))),
    Rock(Set((0, 0), (1, 0), (0, 1), (1, 1)))
  )

case class Chamber(width: Int = 7):
  private val occupied: mutable.Set[(Int, Int)] = mutable.Set.empty

  def highestPoint: Int = occupied.map(_._2).maxOption.getOrElse(-1)

  def render(height: Int): String =
    val maxY = highestPoint
    val minY = maxY - height + 1
    (minY to maxY).reverse
      .map { y =>
        (0 until width).map { x =>
          if occupied.contains((x, y)) then '#' else ' '
        }.mkString
      }
      .mkString("\n")

  def addRock(rock: Rock, jetstream: LazyList[JetDirection]) =
    val r1 = rock.placeAt(2, this.highestPoint + 3)
    // val newPositions = rock.placeAt(2, this.highestPoint + 3).shape
    // if newPositions.exists((x, y) =>
    //     x < 0 || x >= width || y < 0 || occupied.contains((x, y))
    //   )
    // then false // Collision
    // else
    //   occupied ++= newPositions
    //   true

def day17: Unit = {
  val rocks: LazyList[Rock] =
    LazyList.continually(Rocks.rocks).flatten

  // val input = Source.fromFile("inputs/day-17").getLines
  val input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  val jetstream: LazyList[JetDirection] =
    LazyList.continually(input).flatten.map {
      case '>' => Right
      case '<' => Left
    }

  val chamber = Chamber()
}
