package year2024

import cats.parse.{Parser, Rfc5234}

import scala.io.Source
import scala.collection.mutable

def day19: Unit = {
  val (choiceInput, _) = List(
    ("resources/2024/day-19", 0),
    ("resources/2024/day-19a", 0)
  )(1)

  val source = Source.fromFile(choiceInput)
  val input = source.getLines().mkString("\n")
  source.close()

  val p_pattern = Parser.charIn("bgruw").rep.map(_.toList.mkString)
  val p_towels = p_pattern
    .repSep(Parser.string(", "))
    .map(_.toList.toSet)
  val p_designs = p_pattern
    .repSep(Rfc5234.lf)
    .map(_.toList)
  val parser =
    (p_towels <* Rfc5234.lf.rep0) ~ (p_designs <* Rfc5234.lf.rep0)

  val (part1, part2) = parser
    .parseAll(input)
    .map { (towels, designs) =>
      val arrangements = designs
        .map(search(_, towels))
      (arrangements.filter(_ > 0).size, arrangements.sum)
    }
    .getOrElse((0, 0.toLong))

  println(part1)
  println(part2)
}

private val searchCache = mutable.Map[String, Long]()
private def search(input: String, candidates: Set[String]): Long =
  searchCache.getOrElseUpdate(
    input,
    candidates.toVector.map { next =>
      if input == next then 1
      else if !input.startsWith(next) then 0
      else search(input.drop(next.size), candidates)
    }.sum
  )
