package year2024

import scala.io.Source

def day24: Unit = {
  val inputFile = List(
    "resources/2024/day-24",
    "resources/2024/day-24a",
    "resources/2024/day-24b"
  )(0)
  val source = Source.fromFile(inputFile)
  val connections = source
    .getLines()
    .map(_.split("-"))
    .collect { case Array(from, to) =>
      (from, to)
    }
    .toVector
  source.close()

  val memory: Map[String, Boolean] = ???

  case class Gate(val input1: String, val input2: String, output: String) {
    def hasInput = memory.contains(input1) && memory.contains(input1)
  }

  case class AndGate(val input1: String, val input2: String, output: String) extends Gate {
    def process = memory(input1) && memory(input2)
  }

  case class OrGate(val input1: String, val input2: String, output: String) extends Gate {
    def process = memory(input1) || memory(input2)
  }

  case class XorGate(val input1: String, val input2: String, output: String) extends Gate {
    def process = memory(input1) ^ memory(input2)
  }

  // First block: Initial values
  //  -> Map(name, val)
  //
  // Second block:
  //   Set of instructions that wait until both inputs are available and
  //   produces new values
  //  -> 

  // a AND b => 1 if both 1, else 0
  // a  OR b => 1 if either 1, else 0
  // a XOR b => 1 if different, else 0

  println(part1)
  println(part2)
}
