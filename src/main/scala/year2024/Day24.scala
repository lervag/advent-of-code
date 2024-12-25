package year2024

import cats.parse.{Parser, Rfc5234}

import scala.collection.mutable
import scala.collection.mutable.{Queue, Stack}
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
        Parser.string(" AND ").as("and")
          | Parser.string(" OR ").as("or")
          | Parser.string(" XOR ").as("xor")
      )
      ~ p_alnums
      ~ (Parser.string(" -> ") *> p_alnums))
      .map { case (((((in1, t), in2), out))) =>
        Gate(in1, in2, out, t)
      }
  val parser =
    (p_value.repSep0(Rfc5234.lf) <* Rfc5234.lf.rep0) ~ (p_gate.repSep0(
      Rfc5234.lf
    ) <* Rfc5234.lf.rep0)

  parser
    .parseAll(input)
    .map { case (values, gates) =>
      Circuit.setSignals(values)
      Circuit.setGates(gates)
      Circuit.process()
    }
  val part1 = Circuit.getValue("z")
  println(part1)

  // Del 2: Kretsen vil regne ut summen av x og y
  //   Men: 4 par av gates har byttet output
  //   Jeg skal finne hvilke 8 outputs som er byttet og gi svaret i sortert
  //   rekkefølge

  // Det jeg gjorde var å inspisere resultatet av kretsen sammenlignet med
  // riktig beregnet x + y. Ved å variere x og y kunne jeg lokalisere
  // z-punktene der det blir feil.

  // val x: Long = 0b111111111011111111100111111110011100111111111
  // val y: Long = 0b000000001000000000100000000010000100000000001
  // Circuit.setInput(x, y)
  // Circuit.process()
  // println(x + y)
  // println(Circuit.getValue("z"))

  // Deretter tegnet jeg opp krets-trær fra disse og omliggende z-verdier. Ved
  // å se på disse var det tydelig at noen av dem var byttet om. Jeg fant raskt
  // ut at patternet for gates skal være ca slik: ^ -> | ^ -> & & -> ^ |

  // Circuit.drawTree("z13")
  // Circuit.drawTree("z14")
  // Circuit.drawTree("z15")

  // Da fant jeg altså følgende:
  //
  // z10 <- vrn & dkr
  // kmb <- vrn ^ dkr
  // z15 <- fsh | jkh
  // tvp <- kvg ^ qts
  // z25 <- y25 & x25
  // dpg <- bpw ^ dgv
  // mmf <- x35 & y35
  // vdk <- x35 ^ y35
  //
  // Oppsamlet gir dette følgende svar:
  println("dpg,kmb,mmf,tvp,vdk,z10,z15,z25")
}

private case class Gate(
    input1: String,
    input2: String,
    output: String,
    funcName: String
) {
  val funcSymb = funcName match {
    case "and" => "&"
    case "or" => "|"
    case "xor" => "^"
  }
  override def toString(): String = f"$output = $input1 $funcSymb $input2"
}

private object Circuit {
  private var memory: Map[String, Boolean] = Map()
  private var gates: List[Gate] = Nil

  def drawTree(node: String) = {
    val stack = Stack[(Gate, Int)]()
    stack ++= gates.filter(_.output == node).map((_, 0))
    val visited = mutable.Set[Gate]()

    var i = 0
    while (stack.nonEmpty && i < 15) {
      val (gate, depth) = stack.pop()
      if !visited.contains(gate) then
        visited += gate
        println((" ").repeat(depth) + gate)
        stack ++= gates.filter(_.output == gate.input1).map((_, depth + 2))
        stack ++= gates.filter(_.output == gate.input2).map((_, depth + 2))
      i += 1
    }
  }

  def getValue(name: "x" | "y" | "z") =
    memory.toVector
      .filter(_._1.startsWith(name))
      .sorted
      .map(_._2)
      .foldRight(0.toLong)((b, i) => (i << 1) + (if (b) 1 else 0))

  def process() = {
    val queue = Queue[Gate]() ++ gates

    while (queue.nonEmpty) {
      val gate = queue.dequeue()
      if hasInputs(gate) then processGate(gate)
      else queue += gate
    }
  }

  def processGate(gate: Gate) =
    if hasInputs(gate) then
      val input1Value = memory(gate.input1)
      val input2Value = memory(gate.input2)
      val result = gate.funcName match {
        case "and" => input1Value && input2Value
        case "or" => input1Value || input2Value
        case "xor" => input1Value ^ input2Value
      }
      setSignal(gate.output, result)
      true
    else false

  def setGates(gatesIn: List[Gate]): Unit = gates = gatesIn

  def setInput(x: Long, y: Long) = {
    println(y)
    memory = Map()
    val xbin = x.toBinaryString.reverse
    val ybin = y.toBinaryString.reverse
    (0 until 45) foreach { i =>
      setSignal(f"x$i%02d", i < xbin.size && xbin(i) == '1')
      setSignal(f"y$i%02d", i < ybin.size && ybin(i) == '1')
    }
  }

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
