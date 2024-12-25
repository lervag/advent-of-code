package year2024

import cats.parse.{Parser, Rfc5234}

import scala.io.Source

def day25: Unit = {
  val inputFile = List(
    "resources/2024/day-25",
    "resources/2024/day-25a"
  )(1)
  val source = Source.fromFile(inputFile)
  val input = source.getLines().mkString("\n")
  source.close()

  val p_block = Parser.charIn(".#").rep.repSep(Rfc5234.lf).map { charM =>
    charM.toList.toVector
      .map(_.toList.toVector)
      .transpose
      .map(v => v.count(_ == '#'))
  }
  val p_lock = (Parser.char('#').rep ~ Rfc5234.lf) *> p_block.map(Lock(_))
  val p_key = p_block.map { v => Key(v.map(_ - 1)) }
  val parser = (p_lock | p_key).repSep0(Rfc5234.lf.rep)

  val part1 = parser
    .parseAll(input)
    .map { items =>
      val keys = items.collect { case k: Key => k }
      val locks = items.collect { case l: Lock => l }
      val x = locks.flatMap { l =>
        keys.filter { k =>
          l.heights.zip(k.heights).forall(_ + _ <= 5)
        }
      }
      x.size
    }
    .getOrElse(0)
  println(part1)
}

private case class Lock(val heights: Vector[Int])
private case class Key(val heights: Vector[Int])
