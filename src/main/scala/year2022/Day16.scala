package year2022

import scala.collection.mutable.ListBuffer
import scala.io.Source

def day16: Unit = {
  val mapOfValves = Source
    .fromFile("resources/input-day-16a")
    .getLines
    .toVector
    .map(_ match {
      case s"Valve $n has flow rate=$r; tunnels lead to valves $v" =>
        Valve(n, r.toInt, v.split(", ").toVector)
      case s"Valve $n has flow rate=$r; tunnel leads to valve $v" =>
        Valve(n, r.toInt, Vector(v))
      case _ => Valve("_", 0, Vector[String]())
    })
    .map(v => v.name -> v)
    .toMap

  println(getOptimalRoute(20, mapOfValves))
}

//   -----------------i---
//  /                     \
// a------B-------C       J
// \         /
//  \       /              g--H
//   \     /               |
//    D------------E-------f

sealed case class Valve(name: String, rate: Int, tunnels: Vector[String])

sealed case class State(
    current: Valve,
    steps: Vector[String] = Vector(),
    released: Int = 0,
    releasing: Int = 0,
    opened: Vector[String] = Vector()
) {
  def canOpen: Boolean = current.rate > 0 && !opened.contains(current.name)

  override def toString(): String =
    s"Valves ${opened.mkString(", ")} are open: releasing/released = ${releasing}/${released}."
}

private def move(state: State, target: String, map: Map[String, Valve]) =
  state.copy(
    steps = state.steps ++ Vector(s"You move to valve ${target}"),
    released = state.released + state.releasing,
    current = map(target)
  )

private def open(state: State) =
  state.copy(
    steps = state.steps ++ Vector(s"You open valve ${state.current.name}"),
    released = state.released + state.releasing,
    opened = state.opened ++ Vector(state.current.name),
    releasing = state.releasing + state.current.rate
  )

private def getOptimalRoute(totalTime: Int, map: Map[String, Valve]): State = {
  var currentStates = ListBuffer(State(map("AA")))
  (totalTime to 0 by -1) foreach { time =>
    var nextStates = ListBuffer[State]()

    currentStates.foreach { state =>
      if (state.canOpen) {
        nextStates += open(state)
      }
      state.current.tunnels.foreach { name =>
        nextStates += move(state, name, map)
      }
    }

    currentStates = nextStates.distinctBy { s =>
      (s.current.name, s.released, s.releasing, s.opened)
    }
  }
  currentStates.maxBy(_.released)
}
