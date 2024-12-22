package year2024

def day21: Unit = {
  val input0 = List("029A", "980A", "179A", "456A", "379A")
  val input1 = List("480A", "682A", "140A", "246A", "938A")

  // This works for part 1, but it fails terribly for part 2
  def codeToPressSequence(keypads: List[Keypad], code: String) = {
    keypads.foldRight(Vector(code)) { (pad, strings) =>
      strings.flatMap { currentString =>
        currentString
          .map { c => pad.press(c) }
          .foldLeft(Vector("")) { (acc, list) =>
            for {
              prefix <- acc
              item <- list
            } yield prefix + item
          }
      }
    }
  }

  // I think I want a cached depth first approach instead
  def codeToPressSequenceDF(code: String, keypads: List[Keypad]): Int = ???

  // val testing = List("<")
  //   .map { s =>
  //     codeToPressSequenceDF(
  //       s,
  //       List.fill(2)(DirectionalKeypad()) ++ List(NumericKeypad())
  //     )
  //   }
  // println(testing)

  val part1 = input1
    .map { s =>
      val x = codeToPressSequence(
        List(
          DirectionalKeypad(),
          DirectionalKeypad(),
          NumericKeypad()
        ),
        s
      ).map(_.size).min
      val n = s.take(3).toInt
      (n, x)
    }
    .map { (x, y) => x * y }
    .sum

  // val part2 = input1
  //   .map { s =>
  //     val x = codeToPressSequence(
  //       List.fill(2)(DirectionalKeypad()) ++ List(NumericKeypad()),
  //       s
  //     ).map(_.size).min
  //     val n = s.take(3).toInt
  //     (n, x)
  //   }
  //   .map { (x, y) => x * y }
  //   .sum

  println(part1)
  println(part2)
}

private trait Keypad {
  val illegalSpace: Point
  val pathsBetweenButtons: Map[(Char, Char), Vector[String]]

  private var currentButton = 'A'
  def press(button: Char): Vector[String] = {
    val moves = pathsBetweenButtons((currentButton, button))
    currentButton = button
    moves
  }

  def generatePaths(buttons: Vector[(Char, Point)]) = buttons.flatMap {
    (b0, p0) =>
      buttons.map { (b1, p1) =>
        ((b0, b1), getMoves(p0, p1))
      }
  }.toMap

  private def getMoves(p: Point, target: Point): Vector[String] =
    target - p match {
      case (0, 0) => Vector("A")
      case (0, y) =>
        Vector((if y > 0 then "^" else "v").repeat(math.abs(y)) + "A")
      case (x, 0) =>
        Vector((if x > 0 then ">" else "<").repeat(math.abs(x)) + "A")
      case (x, y) =>
        val dx = if x > 0 then (1, 0) else (-1, 0)
        val dy = if y > 0 then (0, 1) else (0, -1)
        val moves = Vector.fill(math.abs(x))(dx) ++ Vector.fill(math.abs(y))(dy)
        moves.permutations
          .filter { prm =>
            val (_, isAllowed) = prm.foldRight((p, true)) {
              case (delta, (pos, x)) =>
                if !x then (pos, x)
                else
                  val next = pos + delta
                  (next, next != illegalSpace)
            }
            isAllowed
          }
          .toVector
          .map(_.map {
            case (1, 0) => '>'
            case (-1, 0) => '<'
            case (0, 1) => '^'
            case (0, -1) => 'v'
          }.mkString.reverse + "A")
    }
}

private case class DirectionalKeypad() extends Keypad {
  val illegalSpace = (0, 1)
  val pathsBetweenButtons = generatePaths(
    Vector(
      ('<', (0, 0)),
      ('v', (1, 0)),
      ('>', (2, 0)),
      ('^', (1, 1)),
      ('A', (2, 1))
    )
  )
}

private case class NumericKeypad() extends Keypad {
  val illegalSpace = (0, 0)
  val pathsBetweenButtons = generatePaths(
    Vector(
      ('0', (1, 0)),
      ('A', (2, 0)),
      ('1', (0, 1)),
      ('2', (1, 1)),
      ('3', (2, 1)),
      ('4', (0, 2)),
      ('5', (1, 2)),
      ('6', (2, 2)),
      ('7', (0, 3)),
      ('8', (1, 3)),
      ('9', (2, 3))
    )
  )
}
