package year2024

import cats.parse.{Numbers, Parser, Rfc5234}

import scala.io.Source

def day17: Unit = {
  val choiceInput = List(
    ("resources/2024/day-17", "4,6,3,5,6,3,5,2,1,0"),
    ("resources/2024/day-17a", "")
  )(1)

  val source = Source.fromFile(choiceInput._1)
  val input = source.getLines().mkString("\n")
  source.close()

  // val ex1 = Program(List(2, 6), 0, 0, 9).run()
  // val ex2 = Program(List(5, 0, 5, 1, 5, 4), 10, 0, 0).run()
  // val ex3 = Program(List(0, 1, 5, 4, 3, 0), 2024, 0, 0).run()
  // val ex4 = Program(List(1, 7), 0, 29, 0).run()
  // val ex5 = Program(List(4, 0), 0, 2024, 43690).run()
  // val ex6a = Program(List(0, 3, 5, 4, 3, 0), 2024, 0, 0).run()
  // val ex6b = Program(List(0, 3, 5, 4, 3, 0), 117440, 0, 0).run()

  val p_registers =
    ((Parser.string("Register ") ~ Rfc5234.alpha ~ Parser.string(": "))
      *> Numbers.digits.map(_.toInt))
      .repSep0(Rfc5234.lf)
  val p_program =
    Parser.string("Program: ") *> Numbers.digit
      .map(_.asDigit)
      .repSep0(Parser.char(','))
  val parser = (p_registers ~ (Rfc5234.lf.rep *> p_program))
    .map { (regs, in) => Program(in, regs(0), regs(1), regs(2)) }

  val part1 = parser
    .parseAll(input)
    .map { case program =>
      program.run()
    }
    .getOrElse("Brzzzzt... FAIL FAIL FAIL!")
  println(part1)

  val part2 = parser
    .parseAll(input)
    .map { case program =>
      bfsForCoeffs(program, List(5))
        .map(makeGuessFromCoefficients(_))
        .min
    }
    .getOrElse("Brzzzzt... FAIL FAIL FAIL!")
  println(part2)
}

private def makeGuessFromCoefficients(coeffs: List[Int]): Long = coeffs
  .foldLeft((15, 0.toLong)) { case ((k, sum), a) =>
    (k - 1, sum + a * math.pow(8, k).toLong)
  }
  ._2

private def bfsForCoeffs(
    program: Program,
    coefficients: List[Int]
): Set[List[Int]] = {
  val result = program
    .copy(rega = makeGuessFromCoefficients(coefficients))
    .run()
    .split(",")
    .map(_.toInt)
    .toList
  val currentIndex = program.instructions.size - coefficients.size

  if result == program.instructions then Set(coefficients)
  else if program.instructions(currentIndex) != result(currentIndex) then Set()
  else
    (0 until 8).flatMap { k => bfsForCoeffs(program, coefficients :+ k) }.toSet
}

private case class Program(
    instructions: List[Int],
    var rega: Long,
    var regb: Long,
    var regc: Long
) {
  private var pointer: Int = 0
  private var output = ""

  def run() = {
    while (pointer < instructions.size) {
      instructions(pointer) match {
        case 0 => // adv()
          rega = rega / math.pow(2, comboOperand.toDouble).toLong
          pointer += 2
        case 1 => // bxl()
          regb ^= literalOperand
          pointer += 2
        case 2 => // bst()
          regb = comboOperand % 8
          pointer += 2
        case 3 => // jnz()
          if rega != 0 then pointer = literalOperand
          else pointer += 2
        case 4 => // bxc()
          regb ^= regc
          pointer += 2
        case 5 => // out()
          val value = comboOperand % 8
          if output.isEmpty then output += value
          else output += s",$value"
          pointer += 2
        case 6 => // bdv()
          regb = rega / math.pow(2, comboOperand.toDouble).toLong
          pointer += 2
        case 7 => // cdv()
          regc = rega / math.pow(2, comboOperand.toDouble).toLong
          pointer += 2
      }
    }

    output
  }

  private def literalOperand = instructions(pointer + 1)

  private def comboOperand = instructions(pointer + 1) match {
    case 4 => rega
    case 5 => regb
    case 6 => regc
    case i => i.toLong
  }
}
