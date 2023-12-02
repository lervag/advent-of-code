package year2023

import scala.io.Source

def day02: Unit = {
  val source = Source.fromFile("resources/2023/day-02")
  val lines = source.getLines.toVector
  source.close()

  val games = lines.map { _ match {
    case s"Game $n: $__sets" =>
      val sets = __sets.split("; ").toVector.map { colors =>
        var nr = 0
        var nb = 0
        var ng = 0
        colors.split(", ").toVector.foreach { _ match {
          case s"$m red" => nr = m.toInt
          case s"$m blue" => nb = m.toInt
          case s"$m green" => ng = m.toInt
        }}
        (nr, ng, nb)
      }
      (n.toInt, sets)
  }}

  val filteredGames = games.filterNot { game =>
    game._2.exists { (nr, ng, nb) => nr > 12 || ng > 13 || nb > 14 }
  }
  println(s"Sum of game ids: ${filteredGames.map(_._1).sum}")

  val powers = games.map { g =>
    val nr = g._2.map(_._1).max
    val nb = g._2.map(_._2).max
    val ng = g._2.map(_._3).max
    nr*nb*ng
  }
  println(s"Sum of powers: ${powers.sum}")
}
