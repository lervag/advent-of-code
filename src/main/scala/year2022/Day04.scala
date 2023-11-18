import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

private def day04: Unit = {
  def inputToSet(s: String): Set[Int] = s.split('-')
    .toList
    .map(_.toInt) match {
      case List(a, b) => Range(a, b).inclusive.toSet
      case _ => Set[Int]()
    }
  def overlapFull(sets: List[Set[Int]]) = sets match {
    case List(a, b) => (a subsetOf b) || (b subsetOf a)
    case _ => false
  }
  def overlap(sets: List[Set[Int]]) = sets match {
    case List(a, b) => !(a intersect b).isEmpty
    case _ => false
  }
  def sumBool(list: List[Boolean]): Int = list.map(if _ then 1 else 0).sum

  val filename =  "resources/input-day-04"
  val source = Source.fromFile(filename)
  val lines = source.getLines.toList
  source.close

  val inputSets = lines.map (_.split(',').toList.map (inputToSet))
  val sumFull = sumBool(inputSets.map(overlapFull))
  println(s"Number of fully contained pairs: $sumFull")

  val sumPartial = sumBool(inputSets.map(overlap))
  println(s"Number of partially contained pairs: $sumPartial")
}
