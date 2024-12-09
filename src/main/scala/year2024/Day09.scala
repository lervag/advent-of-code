package year2024

import scala.io.Source
import scala.collection.mutable
import cats.parse.{Parser => P, Numbers}

def day09: Unit = {
  val source = Source.fromFile("resources/2024/day-09")
  val input = source.getLines().next()
  source.close()

  val p_int = Numbers.digit.map(_.asDigit)
  val p = (p_int ~ (p_int ~ p_int).rep0(0)).map {
    case (first_filesize, free_filesize_pairs) =>
      (0, first_filesize) +: free_filesize_pairs.toVector
  }
  val parsed = p
    .parseAll(input)
    .getOrElse(Vector())
    .zipWithIndex
    .map { case ((free, count), id) => (free, (id, count)) }

  def checksum(input: Vector[Int]): BigInt = input.zipWithIndex
    .foldLeft(BigInt(0)) { case (checksum, (id, number)) =>
      checksum + id * number
    }

  val part1 = checksum(partition1(parsed))
  println(part1)

  val part2 = checksum(partition2(parsed))
  println(part2)
}

private def partition1(input: Vector[(Int, (Int, Int))]) = {
  val n = input.size
  val (_, (_, size)) = input.last
  input
    .foldLeft((n - 1, size, Vector[Int]())) {
      case ((p0, r0, acc), (free, (id, size))) if p0 < id =>
        (0, 0, acc)
      case ((p0, r0, acc), (free, (id, size))) if p0 == id =>
        val collected = (1 to r0).map(_ => id).toVector
        (0, 0, acc ++ collected)
      case ((p0, r0, acc), (free, (id, size))) =>
        var p = p0
        var r = r0
        val moved = (1 to free).map { _ =>
          val id1 = {
            if r == 0 then
              p -= 1
              val (_, (idd, f)) = input(p)
              r = f - 1
              idd
            else
              val (_, (idd, _)) = input(p)
              r -= 1
              idd
          }

          id1
        }.toVector
        val collected = (0 until size).map(_ => id).toVector
        (p, r, acc ++ moved ++ collected)
    }
    ._3
}

private def partition2(input: Vector[(Int, (Int, Int))]) = {
  val flattened = input
    .flatMap { case (f0, (i0, s0)) => Vector((-1, f0), (i0, s0)) }
    .filterNot { (id, size) => id < 0 && size == 0 }

  flattened
    .foldRight(flattened) {
      case ((current_id, _), acc_result) if current_id < 0 => acc_result
      case ((current_id, current_size), acc_result) =>
        val current_index = acc_result.indexWhere { case (i0, _) =>
          i0 == current_id
        }
        val free_index = acc_result.indexWhere { case (i0, f0) =>
          i0 < 0 && f0 >= current_size
        }
        if free_index < 0 || free_index > current_index then acc_result
        else
          val (_, free_size) = acc_result(free_index)
          if (free_size == current_size) then
            acc_result
              .updated(free_index, (current_id, current_size))
              .patch(current_index, Vector((-1, current_size)), 1)
          else
            acc_result
              .patch(current_index, Vector((-1, current_size)), 1)
              .patch(
                free_index,
                Vector(
                  (current_id, current_size),
                  (-1, free_size - current_size)
                ),
                1
              )
    }
    .flatMap { case (id, count) =>
      (0 until count).map { _ => if id >= 0 then id else 0 }.toVector
    }
}
