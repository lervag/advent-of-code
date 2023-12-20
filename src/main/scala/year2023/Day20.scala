package year2023

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.Queue

def day20: Unit = {
  val source = Source.fromFile("resources/2023/day-20")
  val machine = Machine.fromLines(source.getLines().toVector)
  source.close()

  val (totalLow, totalHigh) = Iterator
    .fill(1000)(machine.pushButton())
    .foldLeft((0, 0)) { case ((sumLow, sumHigh), (low, high, _)) =>
      (sumLow + low, sumHigh + high)
    }
  println(s"Part 1: ${totalLow * totalHigh}")

  machine.reset()
  val cycles = mutable.Map("bh" -> 0, "ns" -> 0, "dl" -> 0, "vd" -> 0)
  var i = 0
  while (cycles.values.exists(_ == 0)) {
    i += 1
    val (_, _, source) = machine.pushButton()
    if (source.size > 0)
      cycles(source) = i
  }
  val part2 = cycles.values
    .foldLeft(BigInt(1)) { (acc, next) => lcm(acc, BigInt(next)) }
  println(s"Part 2: $part2")
}

sealed class Machine(val modules: Map[String, Module]) {
  def pushButton() = {
    var low = 0
    var high = 0
    var source = ""
    val queue = Queue(Pulse(false, "button", "broadcaster"))
    while (queue.nonEmpty) {
      val pulse = queue.dequeue()
      queue.enqueueAll(send(pulse))
      if (pulse.value)
        high += 1
      else
        low += 1
      if (pulse.destination == "zh" && pulse.value)
        source = pulse.source
    }
    (low, high, source)
  }

  def send(pulse: Pulse) = modules(pulse.destination).send(pulse)

  def reset() = modules.values.foreach(_.reset())
}

sealed case class Pulse(value: Boolean, source: String, destination: String) {
  override def toString() =
    s"$source ${if (value) "+>" else "->"} $destination"
}

sealed abstract class Module {
  val name: String
  val destinations: Vector[String]
  def send(pulse: Pulse): Vector[Pulse]
  def reset(): Unit = {}
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
    if (pulse.value) Vector[Pulse]()
    else
      state = !state
      destinations.map { d => Pulse(state, name, d) }
  }

  override def reset() = state = false

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

  override def reset() = mostRecentPulse.map { (key, value) => key -> false }

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
