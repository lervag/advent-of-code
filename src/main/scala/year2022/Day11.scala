package year2022

import scala.collection.mutable.Stack
import scala.io.Source

sealed class Monkey(
    id: Int,
    val items: Stack[Long],
    inspector: (Long) => Long,
    val divisor: Long,
    monkey_true: Int,
    monkey_false: Int
) {
  var inspected: Long = 0

  def inspectItem: Long = {
    inspected += 1
    inspector(items.pop) % 9699690 // 96577
  }

  def targetMonkey(worryLevel: Long): Int =
    if (worryLevel % divisor == 0)
      monkey_true
    else
      monkey_false

  override def toString = f"$id : $inspected%8d $items"
}

def day11: Unit = {
  val source = Source.fromFile("resources/input-day-11")
  val monkeys = source.getLines.grouped(7).toList.map(textToMonkey)
  source.close

  (1 to 10000) foreach { round =>
    for (i <- 0 to monkeys.length - 1) {
      while (!monkeys(i).items.isEmpty) {
        val worryLevel = monkeys(i).inspectItem
        val target = monkeys(i).targetMonkey(worryLevel)
        monkeys(target).items.addOne(worryLevel)
      }
    }
  }

  monkeys foreach println

  monkeys.map(_.inspected).sorted.takeRight(2) foreach println
  val monkeyBusiness =
    monkeys.map(_.inspected).sorted.takeRight(2).reduce(_ * _)
  println(s"Monkey business: $monkeyBusiness")
}

private def textToMonkey(text: Seq[String]): Monkey = {
  var items = Stack[Long]()
  var inspector = (old: Long) => old
  var id = 0
  var divisor: Long = 0
  var monkey_true = 0
  var monkey_false = 0

  text map (_ match {
    case s"Monkey $d:" => id = d.toInt
    case s"  Starting items: $d" =>
      items = Stack.from(d.split(", ").map(_.toLong))
    case "  Operation: new = old * old" =>
      inspector = (old: Long) => old * old
    case s"  Operation: new = old * $d" =>
      inspector = (old: Long) => old * d.toLong
    case s"  Operation: new = old + $d" =>
      inspector = (old: Long) => old + d.toLong
    case s"  Test: divisible by $d"          => divisor = d.toLong
    case s"    If true: throw to monkey $d"  => monkey_true = d.toInt
    case s"    If false: throw to monkey $d" => monkey_false = d.toInt
    case _                                   => None
  })

  new Monkey(id, items, inspector, divisor, monkey_true, monkey_false)
}
