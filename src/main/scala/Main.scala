import year2023._

@main def main: Unit = {
  val before = System.currentTimeMillis
  day17
  val after = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
}
