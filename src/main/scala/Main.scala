import year2024._

@main def main: Unit = {
  val before = System.currentTimeMillis
  day05
  val after = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
}
