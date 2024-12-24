package year2024

import cats.parse.{Parser, Rfc5234}

import scala.collection.mutable.Queue
import scala.io.Source

def day24: Unit = {
  val inputFile = List(
    "resources/2024/day-24",
    "resources/2024/day-24a",
    "resources/2024/day-24b"
  )(2)
  val source = Source.fromFile(inputFile)
  val input = source.getLines().mkString("\n")
  source.close()

  val p_alnums = (Rfc5234.alpha | Rfc5234.digit).rep.string
  val p_value = (p_alnums ~ (Parser.string(": ") *> Rfc5234.bit.map(_ == '1')))
  val p_gate =
    (p_alnums
      ~ (
        Parser.string(" AND ").as((a: Boolean, b: Boolean) => a && b)
          | Parser.string(" OR ").as((a: Boolean, b: Boolean) => a || b)
          | Parser.string(" XOR ").as((a: Boolean, b: Boolean) => a ^ b)
      )
      ~ p_alnums
      ~ (Parser.string(" -> ") *> p_alnums))
      .map { case (((((in1, fn), in2), out))) =>
        Gate(in1, in2, out, fn)
      }
  val parser =
    (p_value.repSep0(Rfc5234.lf) <* Rfc5234.lf.rep0) ~ (p_gate.repSep0(
      Rfc5234.lf
    ) <* Rfc5234.lf.rep0)

  val (x, y, z) = parser
    .parseAll(input)
    .map { case (values, gates) =>
      Circuit.setSignals(values)
      Circuit.processGates(gates)
      (Circuit.getValue("x"), Circuit.getValue("y"), Circuit.getValue("z"))
    }
    .getOrElse((0.toLong, 0.toLong, 0.toLong))
  println(z)

  // 4 par av gates har byttet output
  // der er 222 gates og det er UMULIG å brute force her
  //
  // x + y = z
  val z0 = x + y
  val r = z - x - y
  println(f"${z0.toBinaryString}%46s")
  println(f"${z.toBinaryString}%46s")
  println(f"${r.toBinaryString}%46s")

  //
  // println(part2)
}

private case class Gate(
    input1: String,
    input2: String,
    output: String,
    gateFunc: (Boolean, Boolean) => Boolean
) {
  override def toString(): String = s"Gate ($input1, $input2) -> $output"
}

private object Circuit {
  private var memory: Map[String, Boolean] = Map()

  def getValue(name: "x" | "y" | "z") =
    memory.toVector
      .filter(_._1.startsWith(name))
      .sorted
      .map(_._2)
      .foldRight(0.toLong)((b, i) => (i << 1) + (if (b) 1 else 0))

  def processGates(gates: List[Gate]) = {
    val queue = Queue[Gate]() ++ gates

    var i = 0
    while (queue.nonEmpty && i < 10000) {
      val gate = queue.dequeue()
      if hasInputs(gate) then processGate(gate)
      else queue += gate
      i += 1
    }
  }

  def processGate(gate: Gate) =
    if hasInputs(gate) then
      val input1Value = memory(gate.input1)
      val input2Value = memory(gate.input2)
      val result = gate.gateFunc(input1Value, input2Value)
      setSignal(gate.output, result)
      true
    else false

  def setSignals(signals: List[(String, Boolean)]): Unit =
    signals.foreach { (name, value) => setSignal(name, value) }

  def setSignal(name: String, value: Boolean): Unit = {
    memory = memory.updated(name, value)
  }

  def hasInputs(gate: Gate): Boolean = {
    memory.contains(gate.input1) && memory.contains(gate.input2)
  }

  def getOutput(name: String): Option[Boolean] = {
    memory.get(name)
  }
}
