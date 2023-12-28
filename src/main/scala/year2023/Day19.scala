package year2023

import scala.io.Source
import scala.collection.mutable

def day19(): Unit = {
  val source = Source.fromFile("resources/2023/day-19")
  val separatedLines = source.getLines().toVector
    .foldRight(Vector(Vector.empty[String])) { (line, acc) =>
      if (line.isEmpty)
        Vector.empty[String] +: acc
      else
        (line +: acc.head) +: acc.tail
    }
    .filter(_.nonEmpty)
  source.close()

  val workflowMap = parseWorkflows(separatedLines(0)).toMap
  val parts = parseParts(separatedLines(1))

  val part1 = parts.flatMap { part =>
    var workflow = "in"
    while (workflow.size > 1) {
      workflow = workflowMap(workflow)
        .collectFirst { case wf if wf.checkPart(part) => wf.target }
        .getOrElse("")
    }
    if (workflow == "A")
      Some(part.sum)
    else
      None
  }.sum
  println(s"Part 1: $part1")

  val part2 = runWorkflowsWithRanges(workflowMap)
  println(s"Part 2: $part2")
}

private def runWorkflowsWithRanges(workflowMap: Map[String, Vector[Workflow]]) = {
  val finished = mutable.ArrayBuffer.empty[RangedPart]
  val work = mutable.Queue(RangedPart(
    mutable.Map(
      "x" -> (1 to 4000),
      "m" -> (1 to 4000),
      "a" -> (1 to 4000),
      "s" -> (1 to 4000)
    ),
    "in"
  ))

  while (work.nonEmpty) {
    val current = work.dequeue

    if (current.wf == "A")
      finished += current
    else if (current.wf != "R")
      workflowMap(current.wf).foldLeft(current) { (c, workflow) =>
        workflow.handleRangedPart(c) match {
          case (lower, higher) if lower.wf == current.wf =>
            work.enqueue(higher)
            lower
          case (lower, higher) =>
            work.enqueue(lower)
            higher
          case single: RangedPart =>
            if (single.wf != current.wf)
              work.enqueue(single)
            single
        }
      }
  }

  finished.map(_.combinations).sum
}

private def parseWorkflows(lines: Vector[String]) =
  lines.map { case s"$name{$rules}" =>
    val workflows = rules.split(",").toVector.map(_ match {
      case s"$category<$number:$box" => WorkflowLT(box, category, number.toInt)
      case s"$category>$number:$box" => WorkflowGT(box, category, number.toInt)
      case s"$box"                   => WorkflowSimple(box)
    })
    name -> workflows
  }

private def parseParts(lines: Vector[String]) =
  lines.map { case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt) }

case class Part(x: Int, m: Int, a: Int, s: Int) {
  def extract(fieldName: String) = fieldName match {
    case "x" => x
    case "m" => m
    case "a" => a
    case "s" => s
  }

  def sum = x + m + a + s
}

case class RangedPart(ratings: mutable.Map[String, Range], wf: String) {
  def combinations = ratings.values.map(_.size.toLong).reduce(_*_)
  def isEmpty = ratings.values.forall(_.isEmpty)
}

trait Workflow {
  val target: String
  def checkPart(part: Part): Boolean = true
  def handleRangedPart(part: RangedPart): RangedPart | (RangedPart, RangedPart) = part.copy(wf = target)
}

case class WorkflowSimple(target: String) extends Workflow

case class WorkflowGT(target: String, category: String, threshold: Int) extends Workflow {
  override def checkPart(part: Part): Boolean = part.extract(category) > threshold

  override def handleRangedPart(part: RangedPart): RangedPart | (RangedPart, RangedPart) = {
    val range = part.ratings(category)
    val splitPos = threshold - range.start + 1
    val (lower, upper) = range.splitAt(splitPos)

    val partLower = part.copy(ratings = part.ratings.clone())
    partLower.ratings(category) = lower

    val partHigher = part.copy(wf = target, ratings = part.ratings.clone())
    partHigher.ratings(category) = upper

    if (partLower.isEmpty)
      partHigher
    else if (partHigher.isEmpty)
      partLower
    else
      (partLower, partHigher)
  }
}

case class WorkflowLT(target: String, category: String, threshold: Int) extends Workflow {
  override def checkPart(part: Part): Boolean = part.extract(category) < threshold

  override def handleRangedPart(part: RangedPart): RangedPart | (RangedPart, RangedPart) = {
    val range = part.ratings(category)
    val splitPos = threshold - range.start
    val (lower, upper) = range.splitAt(splitPos)

    val partLower = part.copy(wf = target, ratings = part.ratings.clone())
    partLower.ratings(category) = lower

    val partHigher = part.copy(ratings = part.ratings.clone())
    partHigher.ratings(category) = upper

    if (partLower.isEmpty)
      partHigher
    else if (partHigher.isEmpty)
      partLower
    else
      (partLower, partHigher)
  }
}
