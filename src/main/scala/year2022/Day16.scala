package year2022

import scala.io.Source
import scala.collection.mutable

//                          21
//   -----------------i-----J
//  /     13      2
// a------B-------C
// \         /
//  \       /
//   \20   /       3                     22
//    D------------E-------f------g------H

def day16: Unit = {
  val valves = mutable.Map[String, Integer]()
  val tunnels = mutable.Map[String, Vector[String]]()
  Source
    .fromFile("resources/input-day-16")
    .getLines
    .foreach(_ match {
      case s"Valve $n has flow rate=$r; tunnels lead to valves $v" =>
        valves(n) = r.toInt
        tunnels(n) = v.split(", ").toVector
      case s"Valve $n has flow rate=$r; tunnel leads to valve $v" =>
        valves(n) = r.toInt
        tunnels(n) = Vector(v)
    })

  val distances = mutable.Map[String, mutable.Map[String, Integer]]()
  valves
    .filter((id, rate) => id == "AA" || rate > 0)
    .foreach { (id, _) =>
      distances(id) = mutable.Map[String, Integer]((id -> 0), ("AA" -> 0))

      val visited = mutable.Set(id)
      val queue = mutable.Queue[(String, Integer)]((id, 0))

      while (queue.size > 0) {
        val (currentId, currentDistance) = queue.dequeue
        for (neighbourId <- tunnels(currentId).filterNot(visited.contains(_))) {
          visited.add(neighbourId)
          if (valves(neighbourId) > 0) {
            distances(id)(neighbourId) = currentDistance + 1
          }
          queue += ((neighbourId, currentDistance + 1))
        }
      }

      distances(id).remove("AA")
      if (id != "AA") distances(id).remove(id)
    }

  val indices = distances.keys.zipWithIndex.toMap
  val cache = mutable.Map[(Integer, String, Int), Integer]()
  println(dfs(30, "AA", 0, valves, distances, indices, cache))

  val before = System.currentTimeMillis
  var maxFlow = 0
  val allClosed = (1 << indices.size) - 1
  (0 to allClosed / 2).foreach { i =>
    maxFlow = math.max(
      maxFlow,
      dfs(26, "AA", i, valves, distances, indices, cache)
        + dfs(26, "AA", allClosed ^ i, valves, distances, indices, cache)
    )
  }
  println(maxFlow)
  val after = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
}

private def dfs(
    time: Integer,
    valve: String,
    opened: Int,
    valves: mutable.Map[String, Integer],
    distances: mutable.Map[String, mutable.Map[String, Integer]],
    indices: Map[String, Int],
    cache: mutable.Map[(Integer, String, Int), Integer]
): Integer = {
  if cache.contains((time, valve, opened)) then
    return cache((time, valve, opened))
  var maxFlow = 0

  for ((neighbour, distance) <- distances(valve)) {
    val bit = 1 << indices(neighbour)
    if ((opened & bit) == 0) {
      val remtime = time - distance - 1
      if (remtime > 0) {
        maxFlow = math.max(
          maxFlow,
          remtime * valves(neighbour) + dfs(
            remtime,
            neighbour,
            (opened | bit),
            valves,
            distances,
            indices,
            cache
          )
        )
      }
    }
  }

  cache((time, valve, opened)) = maxFlow
  return maxFlow
}

def time[T](block: => T): T = {
  val before = System.currentTimeMillis
  val result = block
  val after = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
  result
}
