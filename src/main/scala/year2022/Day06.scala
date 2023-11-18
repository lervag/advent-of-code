import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

private def day06: Unit = {
  def identify(s: String, n: Int): Int = {
    (n to s.length).zipWithIndex
      .map { (i2, i1) => (i1, i2, s.substring(i1, i2).toSet.size) }
      .takeWhile { _._3 < n }
      .last
      ._2 + 1
  }

  val source = Source.fromFile("resources/input-day-06")
  val input = source.getLines.toList(0)
  source.close

  println(s"${identify("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4)} 7")
  println(s"${identify("bvwbjplbgvbhsrlpgdmjqwftvncz", 4)} 5")
  println(s"${identify("nppdvjthqldpwncqszvftbrmjlhg", 4)} 6")
  println(s"${identify("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4)} 10")
  println(s"${identify("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4)} 11")
  println(s"Answer: ${identify(input, 4)}")

  println(s"${identify("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14)} 19")
  println(s"${identify("bvwbjplbgvbhsrlpgdmjqwftvncz", 14)} 23")
  println(s"${identify("nppdvjthqldpwncqszvftbrmjlhg", 14)} 23")
  println(s"${identify("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14)} 29")
  println(s"${identify("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14)} 26")
  println(s"Answer: ${identify(input, 14)}")
}
