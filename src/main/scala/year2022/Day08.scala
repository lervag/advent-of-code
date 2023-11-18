import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

private def day08: Unit = {
  def pprint[A](x: Array[Array[A]]): Unit = {
    x foreach { row => println(row.toList) }
    println("--")
  }

  extension (array: Array[Array[Int]])
    def combineMin(y: Array[Array[Int]]): Array[Array[Int]] = {
      array.zip(y).map {
        (r1, r2) => r1.zip(r2).map  {
          (c1, c2) => math.min(c1, c2)
        }
      }
    }

    def isBigger(y: Array[Array[Int]]): Array[Array[Boolean]] = {
      array.zip(y).map {
        (r1, r2) => r1.zip(r2).map  {
          (c1, c2) => c1 > c2
        }
      }
    }

    def calculateScenicScore: Array[Array[Int]] = {
      array.zipWithIndex.map { (row, i) =>
        row.zipWithIndex.map { (cell, j) =>
          array.calculateScenicScoreAtPos(i, j)
        }
      }
    }

    def calculateScenicScoreAtPos(i: Int, j: Int): Int = {
      val row = array(i)
      val col = array.map(_(j))
      if (i == 0 || j == 0 || i == col.length - 1 || j == row.length - 1)
        0
      else {
        val value = array(i)(j)

        var left = 1
        var k = j - 1
        while (k > 0 && row(k) < value) {
          left += 1
          k -= 1
        }

        var top = 1
        k = i - 1
        while (k > 0 && col(k) < value) {
          top += 1
          k -= 1
        }

        var right = 1
        k = j + 1
        while (k < row.length - 1 && row(k) < value) {
          right += 1
          k += 1
        }

        var bottom = 1
        k = i + 1
        while (k < col.length - 1 && col(k) < value) {
          bottom += 1
          k += 1
        }

        left*right*bottom*top
      }
    }

  extension (array: Array[Array[Boolean]])
    def countTrue: Int = array.flatten.map { b => if (b) 1 else 0 }.sum

  val source = Source.fromFile("resources/input-day-08")
  val input = source.getLines.toArray
  source.close

  val matrix = input.map(_.toCharArray.toArray.map(_.asDigit))
  val maxFromLeft = matrix.map { row =>
    row.scanLeft(-1)(_.max(_)).dropRight(1)
  }
  val maxFromRight = matrix.map { row =>
    row.scanRight(-1)(_.max(_)).drop(1)
  }
  val maxFromTop = matrix.transpose.map { col =>
    col.scanLeft(-1)(_.max(_)).dropRight(1)
  }.transpose
  val maxFromBottom = matrix.transpose.map { col =>
    col.scanRight(-1)(_.max(_)).drop(1)
  }.transpose
  val minVals = maxFromLeft
    .combineMin(maxFromRight)
    .combineMin(maxFromBottom)
    .combineMin(maxFromTop)

  val visible = matrix.isBigger(minVals)
  println(s"Total number: ${visible.countTrue}")
  println(s"Max scenic number: ${matrix.calculateScenicScore.flatten.max}")
}
