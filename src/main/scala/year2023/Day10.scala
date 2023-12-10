package year2023

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

def day10: Unit = {
  val source = Source.fromFile("resources/2023/day-10")
  val board = source.getLines.toVector.map(_.toVector)
  source.close()

  val loop = findLoop(board)
  val part1 = loop.size / 2
  println(s"Part 1: $part1")

  val innerpart = findInside(board, loop)
  val part2 = innerpart.map { r => r.count { i => i } }.sum
  println(s"Part 2: $part2")
}

private def findInside(
    board: Vector[Vector[Char]],
    loop: Vector[(Int, Int)]
): Vector[Vector[Boolean]] = {
  val isInner = ArrayBuffer.fill[Boolean](board.size, board.size)(false)
  val isLoop = ArrayBuffer.fill[Boolean](board.size, board.size)(false)
  loop.foreach { (i, j) => isLoop(i)(j) = true }

  var p = '.'
  var isInside = false
  for {
    i <- (0 to board.size - 1)
    j <- (0 to board.size - 1)
  } {
    if (isLoop(i)(j))
      board(i)(j) match {
        case '|' | 'S' => isInside = !isInside
        case 'L' => p = 'L'
        case 'F' => p = 'F'
        case 'J' =>
          if (p == 'F') isInside = !isInside
          p = '.'
        case '7' =>
          if (p == 'L') isInside = !isInside
          p = '.'
        case _ =>
      }
    else
      isInner(i)(j) = isInside
  }

  // val boxDrawSymbols = Map(
  //   '|' -> '│',
  //   'S' -> '●',
  //   'L' -> '└',
  //   'F' -> '┌',
  //   'J' -> '┘',
  //   '7' -> '┐',
  //   '-' -> '─',
  //   ).withDefaultValue(' ')
  // board.zipWithIndex.map { (r, i) =>
  //   r.zipWithIndex.map { (char, j) =>
  //     if (isInner(i)(j))
  //       '⣿'
  //     else if (isLoop(i)(j))
  //       boxDrawSymbols(board(i)(j))
  //     else
  //       ' '
  //   }
  //   .mkString
  // }.foreach(println)

  isInner.toVector.map(_.toVector)
}

private def findLoop(
    board: Vector[Vector[Char]]
): Vector[(Int, Int)] = {
  val (i0, j0) = (for {
    (row, rowIndex) <- board.zipWithIndex
    (char, colIndex) <- row.zipWithIndex
    if char == 'S'
  } yield (rowIndex, colIndex)).head
  val (i1, j1) = Vector(
    (i0 - 1, j0, Vector('|', '7', 'F')),
    (i0, j0 + 1, Vector('-', '7', 'J')),
    (i0, j0 - 1, Vector('-', 'L', 'F')),
    (i0 + 1, j0, Vector('|', 'L', 'J'))
  )
    .find { (i, j, allowed) => allowed.contains(board(i)(j)) }
    .map { (i, j, _) => (i, j) }
    .getOrElse((0, 0))

  @tailrec
  def findLoopRecurse(
      i: Int,
      j: Int,
      char: Char,
      acc: Vector[(Int, Int)]
  ): Vector[(Int, Int)] = {
    if (char == 'S') acc
    else
      val (nextI, nextJ) = char match {
        case '|' => if (acc.last == (i - 1, j)) (i + 1, j) else (i - 1, j)
        case 'L' => if (acc.last == (i - 1, j)) (i, j + 1) else (i - 1, j)
        case 'J' => if (acc.last == (i - 1, j)) (i, j - 1) else (i - 1, j)
        case '-' => if (acc.last == (i, j - 1)) (i, j + 1) else (i, j - 1)
        case '7' => if (acc.last == (i, j - 1)) (i + 1, j) else (i, j - 1)
        case 'F' => if (acc.last == (i, j + 1)) (i + 1, j) else (i, j + 1)
      }
      findLoopRecurse(
        nextI,
        nextJ,
        board(nextI)(nextJ),
        acc :+ (i, j)
      )
  }

  findLoopRecurse(i1, j1, board(i1)(j1), Vector((i0, j0)))
}
