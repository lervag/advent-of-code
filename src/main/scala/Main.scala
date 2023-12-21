import year2023._

@main def main: Unit = {
  val before = System.currentTimeMillis
  day21
  val after = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
}
