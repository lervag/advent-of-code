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

  val part1 = parser
    .parseAll(input)
    .map { (towels, designs) => designs.filter(search(_, towels)).size }
    .getOrElse(0)

  // val part2 = parser
  //   .parseAll(input)
  //   .map { b =>
  //     bsForTrue(
  //       0,
  //       b.size,
  //       { k => findShortestPath(start, target, b.take(k).toSet) == 0 }
  //     )
  //   }
  //   .getOrElse(0)

  println(part1)
  // println(part2)
}

private val searchCache = mutable.Map[String, Boolean]()
private def search(input: String, candidates: Set[String]): Boolean =
  searchCache.getOrElseUpdate(
    input,
    if candidates.contains(input) then true
    else
      candidates.filter { next =>
        input.startsWith(next)
        && search(input.drop(next.size), candidates)
      }.size > 0
  )
