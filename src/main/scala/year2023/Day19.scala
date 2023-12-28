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

  val workflows = parseWorkflows(separatedLines(0)).toMap
  val parts = parseParts(separatedLines(1))

  val part1 = parts.flatMap { part =>
    var workflow = "in"
    while (workflow.size > 1) {
      workflow = workflows(workflow)
        .collectFirst { case (wf, box) if wf(part) => box }
        .getOrElse("")
    }
    if (workflow == "A")
      Some(part.sum)
    else
      None
  }.sum
  println(s"Part 1: $part1")

  val workflows2 = parseWorkflowsRanged(separatedLines(0)).toMap
  val part2 = runWorkflowsWithRanges(workflows2)
  println(s"Part 2: $part2")
}

private def runWorkflowsWithRanges(workflows: Map[String, Vector[PartMapper => Vector[PartMapper]]]) = {
  val finished = mutable.ArrayBuffer.empty[PartMapper]
  val other = mutable.ArrayBuffer.empty[PartMapper]
  val work = mutable.Queue(PartMapper(
    (1 to 4000),
    (1 to 4000),
    (1 to 4000),
    (1 to 4000),
    "in"
  ))

  while (work.nonEmpty) {
    val current = work.dequeue
    val currentWf = current.wf

    if (currentWf == "A")
      // println(s"Added $current")
      finished += current
    else if (currentWf == "R")
      // println(s"Ignored $current")
      other += current
    else
      // println(s"Running: $currentWf with $current")
      workflows(currentWf).foldLeft(current) { (pp, g) =>
        val newParts = g(pp).filterNot(_.isEmpty)

        newParts.find(_.wf != currentWf) map { n => work.enqueue(n) }

        newParts.find(_.wf == currentWf) match {
          case Some(next) => next
          case None => pp
        }
      }
  }

  finished.map(_.combinations).sum
}

private def parseWorkflows(lines: Vector[String]) =
  lines.map { case s"$name{$rules}" => name -> rules.split(",").toVector.map(_ match {
    case s"$category<$number:$box" => ({ (p: Part) => p.extract(category) < number.toInt }, box)
    case s"$category>$number:$box" => ({ (p: Part) => p.extract(category) > number.toInt }, box)
    case s"$box" => ({ (p: Part) => true }, box)
  })
  }

private def parseWorkflowsRanged(lines: Vector[String]) =
  lines.map { case s"$name{$rules}" => name -> rules.split(",").toVector.map(_ match {
    case s"x>$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.x.splitAt(number.toInt)
      Vector(p.copy(x = lower), p.copy(x = upper, wf = box))
    }
    case s"x<$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.x.splitAt(number.toInt - 1)
      Vector(p.copy(x = lower, wf = box), p.copy(x = upper))
    }
    case s"m>$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.m.splitAt(number.toInt)
      Vector(p.copy(m = lower), p.copy(m = upper, wf = box))
    }
    case s"m<$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.m.splitAt(number.toInt - 1)
      Vector(p.copy(m = lower, wf = box), p.copy(m = upper))
    }
    case s"a>$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.a.splitAt(number.toInt)
      Vector(p.copy(a = lower), p.copy(a = upper, wf = box))
    }
    case s"a<$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.a.splitAt(number.toInt - 1)
      Vector(p.copy(a = lower, wf = box), p.copy(a = upper))
    }
    case s"s>$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.s.splitAt(number.toInt)
      Vector(p.copy(s = lower), p.copy(s = upper, wf = box))
    }
    case s"s<$number:$box" => { (p: PartMapper) =>
      val (lower, upper) = p.s.splitAt(number.toInt - 1)
      Vector(p.copy(s = lower, wf = box), p.copy(s = upper))
    }
    case s"$box" => { (p: PartMapper) => Vector(p.copy(wf = box)) }
  })
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

case class PartMapper(x: Range, m: Range, a: Range, s: Range, wf: String) {
  def combinations = x.size.toLong * m.size.toLong * a.size.toLong * s.size.toLong
  def isEmpty = x.isEmpty || m.isEmpty || a.isEmpty || s.isEmpty
}
