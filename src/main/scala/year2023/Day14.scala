package year2023

import scala.collection.mutable
import scala.io.Source

def day14: Unit = {
  val source = Source.fromFile("resources/2023/day-14")
  val lines = source.getLines.toVector
  source.close()

  val board = lines.map { line => line.toVector }.transpose

  val part1 = calculateLoad(tiltLeft(board))
  println(s"Part 1: $part1")

  val cacheBoard = mutable.Map.empty[Vector[Vector[Char]], Int]
  val cacheLoad = mutable.Map.empty[Int, Int]
  var index = 0
  var previousBoard = board
  while (!cacheBoard.contains(previousBoard)) {
    cacheBoard(previousBoard) = index
    cacheLoad(index) = calculateLoad(previousBoard)
    previousBoard = cycle(previousBoard)
    index += 1
  }
  val first = cacheBoard(previousBoard)
  val position = (1000000000 - first) % (index - first)
  val part2 = cacheLoad(first + position)
  println(s"Part 2: $part2")

  // LEARNING LazyList
  // case class BoardState(board: Vector[Vector[Char]], load: Int, cycles: Int)
  // val boardCache = mutable.Map.empty[Int, Int]
  // val states = LazyList.iterate(BoardState(board, calculateLoad(board), 0)) { state =>
  //   boardCache(state.board.hashCode) = state.cycles
  //   val nextBoard = cycle(state.board)
  //   BoardState(nextBoard, calculateLoad(nextBoard), state.cycles + 1)
  // }
  // states
  //   .takeWhile { s => !boardCache.contains(s.board.hashCode) }
  //   .foreach { s => println((s.load, s.cycles)) }
}

private def tiltLeft(board: Vector[Vector[Char]]) = board.map(tiltVectorLeft)
private def tiltRight(board: Vector[Vector[Char]]) = board.map(tiltVectorRight)

private def cycle(board: Vector[Vector[Char]]) =
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
      case ((acc, i), 'O') =>
        ((acc.take(i) :+ 'O') ++ acc.takeRight(acc.size - i), i)
      case ((acc, i), '.') => (acc :+ '.', i)
      case ((acc, i), chr) => (acc :+ chr, acc.size + 1)
    }._1
  )

private val cacheRight = mutable.Map.empty[Vector[Char], Vector[Char]]
private def tiltVectorRight(v: Vector[Char]) =
  cacheRight.getOrElseUpdate(
    v,
    v.foldLeft((Vector.empty[Char], 0)) {
      case ((acc, i), '.') =>
        ((acc.take(i) :+ '.') ++ acc.takeRight(acc.size - i), i)
      case ((acc, i), 'O') => (acc :+ 'O', i)
      case ((acc, i), chr) => (acc :+ chr, acc.size + 1)
    }._1
  )

private def calculateLoad(board: Vector[Vector[Char]]) =
  board.transpose
    .zip((1 to board.size).reverse)
    .map { (v, load) => v.count(_ == 'O') * load }
    .sum
