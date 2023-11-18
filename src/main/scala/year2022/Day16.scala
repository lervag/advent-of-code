package year2022

import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

def day16: Unit = {
  case class Valve(name: String, rate: Int, tunnels: Vector[String])

  val mapOfValves = Source.fromFile("resources/input-day-16a").getLines.toVector
    .map(_ match {
      case s"Valve $n has flow rate=$r; tunnels lead to valves $v"
        => Valve(n, r.toInt, v.split(", ").toVector)
      case s"Valve $n has flow rate=$r; tunnel leads to valve $v"
        => Valve(n, r.toInt, Vector(v))
      case _
        => Valve("_", 0, Vector[String]())
    })
    .map(v => v.name -> v)
    .toMap

  //   -----------------i---
  //  /                     \
  // a------B-------C       J
  // \         /
  //  \       /              g--H
  //   \     /               |
  //    D------------E-------f

  case class State(current: Valve, steps: Vector[String] = Vector(), released: Int = 0, releasing: Int = 0, opened: Vector[String] = Vector()) {
    def canOpen: Boolean = current.rate > 0 && !opened.contains(current.name)

    override def toString(): String =
      s"Valves ${opened.mkString(", ")} are open: releasing/released = ${releasing}/${released}."
  }

  def move(state: State, target: String) = state.copy(
      steps = state.steps ++ Vector(s"You move to valve ${target}"),
      released = state.released + state.releasing,
      current = mapOfValves(target),
    )

  def open(state: State) =
    state.copy(
      steps = state.steps ++ Vector(s"You open valve ${state.current.name}"),
      released = state.released + state.releasing,
      opened = state.opened ++ Vector(state.current.name),
      releasing = state.releasing + state.current.rate,
      )

  def getOptimalRoute(totalTime: Int): State = {
    var currentStates = ListBuffer(State(mapOfValves("AA")))
    (totalTime to 0 by -1) foreach { time =>
      var nextStates = ListBuffer[State]()

      currentStates.foreach { state =>
        if (state.canOpen) {
          nextStates += open(state)
        }
        state.current.tunnels.foreach { name =>
          nextStates += move(state, name)
        }
      }

      currentStates = nextStates.distinctBy { s => (s.current.name, s.released, s.releasing, s.opened) }
    }
    currentStates.maxBy(_.released)
  }

  println(getOptimalRoute(20))
}
