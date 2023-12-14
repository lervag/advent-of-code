package year2023

import scala.io.Source
import scala.collection.mutable
import scala.annotation.tailrec

def day14: Unit = {
  val source = Source.fromFile("resources/2023/day-14")
  val lines = source.getLines.toVector
  source.close()

  val board = lines.map { line => line.toVector }.transpose

  val part1 = calculateLoad(tiltLeft(board))
  println(s"Part 1: $part1")

  val easy = 1000
  val medium = 1000000
  val hard = 1000000000
  // val part2Board = repeat(10000000, board)(cycle)
  // part2Board.transpose.foreach { v => println(v.mkString) }
  // val part2 = calculateLoad(part2Board)
  // println(s"Part 2: $part2")
}

private def tiltLeft(board: Vector[Vector[Char]]) = board.map(tiltVectorLeft)
private def tiltRight(board: Vector[Vector[Char]]) = board.map(tiltVectorRight)

private val cacheCycle = mutable.Map.empty[Vector[Vector[Char]], Vector[Vector[Char]]]
private def cycle(board: Vector[Vector[Char]]) =
  cacheCycle.getOrElseUpdate(board, cycleAux(board))

private def cycleAux(board: Vector[Vector[Char]]) =
  tiltRight(
    tiltRight(
      tiltLeft(
        tiltLeft(board).transpose
      ).transpose
    ).transpose
  ).transpose

private val cacheLeft = mutable.Map.empty[Vector[Char], Vector[Char]]
private def tiltVectorLeft(v: Vector[Char]) =
  cacheLeft.getOrElseUpdate(
    v,
    v.foldLeft((Vector.empty[Char], 0)) {
      case ((acc, i), 'O') => ((acc.take(i) :+ 'O') ++ acc.takeRight(acc.size - i), i)
      case ((acc, i), '.') => (acc :+ '.', i)
      case ((acc, i), chr) => (acc :+ chr, acc.size + 1)
    }._1
  )

private val cacheRight = mutable.Map.empty[Vector[Char], Vector[Char]]
private def tiltVectorRight(v: Vector[Char]) =
  cacheRight.getOrElseUpdate(
    v,
    v.foldLeft((Vector.empty[Char], 0)) {
      case ((acc, i), '.') => ((acc.take(i) :+ '.') ++ acc.takeRight(acc.size - i), i)
      case ((acc, i), 'O') => (acc :+ 'O', i)
      case ((acc, i), chr) => (acc :+ chr, acc.size + 1)
    }._1
  )

@tailrec
private def repeat[A](n: Int, arg: A)(f: A => A): A =
  if (n <= 0) arg
  else repeat(n - 1, f(arg))(f)

private def calculateLoad(board: Vector[Vector[Char]]) =
  board.transpose
    .zip((1 to board.size).reverse)
    .map { (v, load) => v.count(_ == 'O')*load }
    .sum
