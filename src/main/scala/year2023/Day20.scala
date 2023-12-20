package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.Queue

def day20: Unit = {
  val input = "resources/2023/day-20"
  val exampleA = "resources/2023/day-20a"
  val exampleB = "resources/2023/day-20b"
  val source = Source.fromFile(input)
  val lines = source.getLines().toVector
  source.close()

  val machine1 = Machine.fromLines(lines)
  val (totalLow, totalHigh) = (0 until 1000)
    .map { _ => machine1.pushButton() }
    .foldLeft((0, 0)) { case ((sumLow, sumHigh), (low, high, _)) =>
      (sumLow + low, sumHigh + high)
    }
  println(s"Part 1: ${totalLow*totalHigh}")

  val machine2 = Machine.fromLines(lines)
  var i = 0
  var rx = 0
  while (rx != 1) {
    val (_, _, newRx) = machine1.pushButton()
    i += 1
  }
  println(s"Part 2: $i")
}

sealed class Machine(val modules: Map[String, Module]) {
  def send(pulse: Pulse) = modules(pulse.destination).send(pulse)

  def pushButton() = {
    var lowToRX = 0
    var low = 0
    var high = 0
    val queue = Queue(Pulse(false, "button", "broadcaster"))
    while (queue.nonEmpty) {
      val pulse = queue.dequeue()
      queue.enqueueAll(send(pulse))
        if (pulse.value)
          high += 1
        else
          low += 1
          if (pulse.destination == "rx")
            lowToRX += 1
    }
    (low, high, lowToRX)
  }

  def reportState() =
    modules
      .values
      .flatMap {
        case m: FlipFlop =>
          Some((m.name, if (m.state) 1 else 0))
        case m: Conjunction =>
          Some((m.name, m.mostRecentPulse.values.count { p => p }))
        case _ => None
      }
      .toVector
      .sorted
}

sealed case class Pulse(value: Boolean, source: String, destination: String) {
  override def toString() = {
    val arrow = if (value) "+>" else "->"
    s"$source $arrow $destination"
  }
}

sealed abstract class Module {
  val name: String
  val destinations: Vector[String]

  def send(pulse: Pulse): Vector[Pulse]

  def sendW(pulse: Pulse): Vector[Pulse] = {
    println("  " + pulse)
    send(pulse)
  }
}

sealed case class Broadcaster(destinations: Vector[String]) extends Module {
  val name = "broadcaster"

  def send(pulse: Pulse) =
    destinations.map { d => Pulse(pulse.value, "broadcaster", d) }

  override def toString() = s"$name: ${destinations.mkString(", ")}"
}

sealed case class Untyped(name: String) extends Module {
  val destinations = Vector[String]()

  def send(pulse: Pulse) = Vector[Pulse]()

  override def toString() = s"%$name UNTYPED"
}

sealed case class FlipFlop(name: String, destinations: Vector[String])
    extends Module {
  var state: Boolean = false

  def send(pulse: Pulse) = {
    if (pulse.value)
      Vector[Pulse]()
    else
      state = !state
      destinations.map { d => Pulse(state, name, d) }
  }

  override def toString() = s"%$name $state: ${destinations.mkString(", ")}"
}

sealed case class Conjunction(name: String, destinations: Vector[String])
    extends Module {
  val mostRecentPulse = mutable.Map
    .empty[String, Boolean]
    .withDefaultValue(false)

  def send(pulse: Pulse) =
    mostRecentPulse(pulse.source) = pulse.value
    val output = !mostRecentPulse.values.forall { p => p }
    destinations.map { d =>
      Pulse(output, name, d)
    }

  override def toString() =
    s"&$name $mostRecentPulse: ${destinations.mkString(", ")}"
}

object Machine {
  def fromLines(lines: Vector[String]): Machine = {
    val modules = lines
      .map(_ match {
        case s"broadcaster -> $destinations" =>
          "broadcaster" -> Broadcaster(destinations.split(", ").toVector)
        case s"%$name -> $destinations" =>
          name -> FlipFlop(name, destinations.split(", ").toVector)
        case s"&$name -> $destinations" =>
          name -> Conjunction(name, destinations.split(", ").toVector)
      })
      .toMap
      .to(mutable.Map)

    modules.foreach { (name, module) =>
      module.destinations.foreach { d =>
        if (modules.contains(d))
          modules(d) match {
            case c: Conjunction =>
              c.mostRecentPulse(name) = false
            case _ =>
          }
        else
          modules(d) = Untyped(d)
      }
    }

    new Machine(modules.toMap)
  }
}
