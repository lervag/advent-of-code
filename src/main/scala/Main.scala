import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Queue, ArrayBuffer, Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

import ujson._

@main def day16: Unit = {
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
private def day15: Unit = {
  case class Beacon(x: Int, y: Int)

  case class Sensor(x: Int, y: Int, range: Int, closest_beacon: Beacon) {
    def coverage(row: Int): Range = {
      val r = range - math.abs(y - row)
      x - r to x + r
    }
  }

  def parseSensor(sx: Int, sy: Int, bx: Int, by: Int)
    = Sensor(sx, sy, math.abs(sx - bx) + math.abs(sy - by), Beacon(bx, by))

  def coverage(sensors: List[Sensor], row: Int) = sensors
    .filter { s => math.abs(s.y - row) < s.range }
    .map { s => s.coverage(row) }
    // .foldLeft(Vector[Int]()) { (a, b) => a.union(b) }

  def search(sensors: List[Sensor], maxRow: Int) = {
    var x: Long = -1
    var y: Long = -1
    while (y <= maxRow && x < 0) {
      y += 1
      if (y % 1000 == 0) {
        println(f"Searching row $y%7d of ${maxRow}")
      }

      val covered = Array.fill(maxRow + 1)(false)
      coverage(sensors, y.toInt) foreach { r =>
        java.util.Arrays.fill(
          covered,
          math.max(0, r.start),
          math.min(maxRow, r.end) + 1,
          true
        )
      }

      x = covered.indexOf(false)
    }

    if (x > 0) {
      println(s"Found ($x, $y)!")
      println(s"Answer is: ${4000000*x + y}")
    }
  }

  val row = 10
  val searchSpace = 20
  val source = Source.fromFile("resources/input-day-15a")
  // val row = 2000000
  // val searchSpace = 4000000
  // val source = Source.fromFile("resources/input-day-15")
  val sensors = source.getLines.toList.map(_ match {
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by"
      => parseSensor(sx.toInt, sy.toInt, bx.toInt, by.toInt)
    case _
      => parseSensor(0, 0, 0, 0)
  })
  source.close

  search(sensors, searchSpace)
}

private def day14: Unit = {
  val stoneMap = MutableMap[(Int, Int), Char]()
  val origin = (500, 0)
  stoneMap(origin) = '+'

  def draw(stoneMap: MutableMap[(Int, Int), Char]): Unit = {
    val xs = stoneMap.keys.map(_._1)
    val ys = stoneMap.keys.map(_._2)
    val xrange = xs.min to xs.max
    val yrange = ys.min to ys.max
    yrange foreach { y =>
      xrange foreach { x =>
        print(stoneMap.getOrElse((x, y), ' '))
      }
      print("\n")
    }
  }

  def pourSand(m: MutableMap[(Int, Int), Char], floor: Boolean): Boolean = {
    var (x, y) = origin
    if (m(origin) == 'o') {
      false
    } else {
      val ymax = m.filter(_._2 == '#').keys.map(_._2).max + 2
      var flowing = true
      while (flowing && y < ymax) {
        y += 1
        if (!m.contains((x, y))) {
          // pass
        } else if (!m.contains((x - 1, y))) {
          x -= 1
        } else if (!m.contains((x + 1, y))) {
          x += 1
        } else {
          flowing = false
          m((x, y - 1)) = 'o'
        }
      }

      if (floor && y == ymax) {
        flowing = false
        m((x, y - 1)) = 'o'
      }

      !flowing
    }
  }

  val source = Source.fromFile("resources/input-day-14a")
  source.getLines foreach { line =>
    line
      .split(" -> ")
      .map { points =>
        val coords = points.split(',').map(_.toInt)
        (coords(0), coords(1))
      }
      .reduce { (c1, c2) =>
        val x = c1._1
        val y = c1._2
        if (c2._1 == x) {
          (if (c1._2 < c2._2)
            (c1._2 to c2._2)
          else
            (c2._2 to c1._2)) foreach { z => stoneMap((x, z)) = '#'}
        } else {
          (if (c1._1 < c2._1)
            (c1._1 to c2._1)
          else
            (c2._1 to c1._1)) foreach { z => stoneMap((z, y)) = '#'}
        }
        c2
      }
  }
  source.close

  // draw(stoneMap)
  while (pourSand(stoneMap, true)) { }
  // draw(stoneMap)

  println(stoneMap.values.count(_ == 'o'))
}

private def day13: Unit = {
  def compare(x: ArrayBuffer[Value], y: ArrayBuffer[Value]): String = {
    if (x.isEmpty && y.isEmpty)
      "continue"
    else if (x.isEmpty)
      "right"
    else if (y.isEmpty)
      "wrong"
    else
      val state = (x.remove(0), y.remove(0)) match {
        case (a1: Arr, b1: Arr) => compare(a1.arr.clone, b1.arr.clone)
        case (a1: Num, b1: Num) =>
          if (a1.num < b1.num)
            "right"
          else if (a1.num > b1.num)
            "wrong"
          else
            "continue"
        case (a1: Num, b1: Arr) => compare(Arr(a1).arr, b1.arr.clone)
        case (a1: Arr, b1: Num) => compare(a1.arr.clone, Arr(b1).arr)
        case _ => "continue"
      }

      if (state == "continue")
        compare(x, y)
      else
        state
  }

  val source = Source.fromFile("resources/input-day-13")
  val pairs = source.getLines.filter(!_.isEmpty)
    .map(read(_))
    .toList
  source.close

  val countRight = pairs
    .grouped(2)
    .map { seq => compare(seq(0).arr.clone, seq(1).arr.clone) }
    .zipWithIndex
    .filter { (string, index) => string == "right" }
    .map(_._2 + 1)
    .sum

  println(countRight)

  val divider1 = read("[[2]]")
  val divider2 = read("[[6]]")
  val pairsExtended = pairs ++ List(divider1, divider2)
  val sortedPairs = pairsExtended.sortWith { (left, right) =>
    compare(left.arr.clone, right.arr.clone) match {
      case "right" => true
      case _ => false
    }
  }

  println((sortedPairs.indexOf(divider1) + 1) * (sortedPairs.indexOf(divider2) + 1))
}

private def day12: Unit = {
  type Index = (Int, Int)

  extension (array: Array[Array[Int]])
    def getIndices(x: Int): Array[Index] = for (
        (row, i) <- array.zipWithIndex;
        (cell, j) <- row.zipWithIndex if cell == x
      ) yield (i, j)

    def get(ind: Index): Int = array(ind._1)(ind._2)

    def search(ind: Index): List[Index] = {
      val directions = List[Index](
        (ind._1 - 1, ind._2),
        (ind._1 + 1, ind._2),
        (ind._1, ind._2 + 1),
        (ind._1, ind._2 - 1)
      )
      val value = array.get(ind)
      val list = for (
        (i, j) <- directions
                    if   i >= 0 && i < array.size
                      && j >= 0 && j < array(0).size
                      && array(i)(j) <= value + 1
      ) yield (i, j)
      list
    }

  def fewestSteps(start: Index, end: Index, map: Array[Array[Int]]): Int = {
    val visited = MutableMap[Index, (Index, Int)]()
    val candidates = Stack[(Index, Int)]((start, -1))
    var previous = (-1, -1)

    while (candidates.nonEmpty && !visited.contains(end)) {
      val (current, score) = candidates.pop
      visited += (current -> (previous, score + 1))
      candidates.addAll(
        map.search(current).filter { c =>
          !visited.contains(c) && !candidates.map(_._1).contains(c)
        }
          .map((_, score + 1)))
      previous = current
    }

    if (visited.contains(end))
      visited(end)._2
    else
      100000
  }

  val source = Source.fromFile("resources/input-day-12")
  val mapRaw = source.getLines.toArray.map(_.toCharArray.map(_.toInt - 96))
  source.close

  val startInitial = mapRaw.getIndices(-13)(0)
  val end = mapRaw.getIndices(-27)(0)

  val map = mapRaw.map(_.map { value =>
    value match {
      case -13 => 1
      case -27 => 26
      case x => x
    }
  })

  println(fewestSteps(startInitial, end, map))

  map.getIndices(1).map { ind =>
    val steps = fewestSteps(ind, end, map)
    if (steps < 100000) println(s"$ind $steps")
    steps
  }

  println(map.getIndices(1).map { ind => fewestSteps(ind, end, map) }.min)
}

private def day11: Unit = {
  class Monkey(
    id: Int,
    val items: Stack[Long],
    inspector: (Long) => Long,
    val divisor: Long,
    monkey_true: Int,
    monkey_false: Int) {
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

  def textToMonkey(text: Seq[String]): Monkey = {
    var items = Stack[Long]()
    var inspector = (old: Long) => old
    var id = 0
    var divisor: Long = 0
    var monkey_true = 0
    var monkey_false = 0

    text map (_ match {
        case s"Monkey $d:" => id = d.toInt
        case s"  Starting items: $d" => items = Stack.from(d.split(", ").map(_.toLong))
        case  "  Operation: new = old * old" => inspector = (old: Long) => old * old
        case s"  Operation: new = old * $d" => inspector = (old: Long) => old * d.toLong
        case s"  Operation: new = old + $d" => inspector = (old: Long) => old + d.toLong
        case s"  Test: divisible by $d" => divisor = d.toLong
        case s"    If true: throw to monkey $d" => monkey_true = d.toInt
        case s"    If false: throw to monkey $d" => monkey_false = d.toInt
        case _ => None
      }
    )

    new Monkey(id, items, inspector, divisor, monkey_true, monkey_false)
  }

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
  val monkeyBusiness = monkeys.map(_.inspected).sorted.takeRight(2).reduce(_ * _)
  println(s"Monkey business: $monkeyBusiness")
}

private def day10: Unit = {
  val source = Source.fromFile("resources/input-day-10")
  val instructions = source.getLines.map{ line =>
    line.split(" ") match {
      case Array("addx", value: String) => (2, value.toInt)
      case Array("noop") => (1, 0)
      case _ => (0, 0)
    }
  }.toArray
  source.close

  var cycle = 1
  var sprite = 1
  val result = for (
    (cycles, add) <- instructions;
    i <- (1 to cycles).reverse
  ) yield {
    cycle += 1
    val pos = (cycle - 2) % 40
    val lit = if (math.abs(sprite - pos) <= 1) '#' else '.'
    if (i == 1) sprite += add
    (cycle, pos, sprite, lit, cycle*sprite)
  }

  val x = result.filter { tuple => (tuple._1 + 20) % 40 == 0 }
  x foreach println
  println(x.map(_._5).sum)

  result.map(_._4).grouped(40).map(_.mkString) foreach println
}

private def day09: Unit = {
  def moveTail(t: (Int, Int), h: (Int, Int)): (Int, Int) = {
    if (t._1 < h._1 - 1)
      if (t._2 < h._2 - 1)
        (h._1 - 1, h._2 - 1)
      else if (t._2 > h._2 + 1)
        (h._1 - 1, h._2 + 1)
      else
        (h._1 - 1, h._2)
    else if (t._1 > h._1 + 1)
      if (t._2 < h._2 - 1)
        (h._1 + 1, h._2 - 1)
      else if (t._2 > h._2 + 1)
        (h._1 + 1, h._2 + 1)
      else
        (h._1 + 1, h._2)
    else if (t._2 < h._2 - 1)
      (h._1, h._2 - 1)
    else if (t._2 > h._2 + 1)
      (h._1, h._2 + 1)
    else
      t
  }

  val source = Source.fromFile("resources/input-day-09")
  val input = source.getLines.toArray
  source.close

  val steps = input
    .map(_.split(" ") match {
        case Array(a, b) => (a, b.toInt)
        case _ => ("", 0)
      })
    .flatMap { (a, b) => List.fill(b)(a) }

  val visited = MutableMap[(Int, Int), Boolean]()
  var posH = (0, 0)
  var pos1 = (0, 0)
  var pos2 = (0, 0)
  var pos3 = (0, 0)
  var pos4 = (0, 0)
  var pos5 = (0, 0)
  var pos6 = (0, 0)
  var pos7 = (0, 0)
  var pos8 = (0, 0)
  var pos9 = (0, 0)
  visited(pos9) = true

  var i = 0
  steps foreach { step =>
    i += 1
    posH = step match {
      case "L" => (posH._1 - 1, posH._2)
      case "D" => (posH._1, posH._2 - 1)
      case "R" => (posH._1 + 1, posH._2)
      case "U" => (posH._1, posH._2 + 1)
      case _ => (0, 0)
    }
    pos1 = moveTail(pos1, posH)
    pos2 = moveTail(pos2, pos1)
    pos3 = moveTail(pos3, pos2)
    pos4 = moveTail(pos4, pos3)
    pos5 = moveTail(pos5, pos4)
    pos6 = moveTail(pos6, pos5)
    pos7 = moveTail(pos7, pos6)
    pos8 = moveTail(pos8, pos7)
    pos9 = moveTail(pos9, pos8)
    visited(pos9) = true
  }

  println(visited.size)
}

private def day08: Unit = {
  def pprint[A](x: Array[Array[A]]): Unit = {
    x foreach { row => println(row.toList) }
    println("--")
  }

  extension (array: Array[Array[Int]])
    def combineMin(y: Array[Array[Int]]): Array[Array[Int]] = {
      array.zip(y).map {
        (r1, r2) => r1.zip(r2).map  {
          (c1, c2) => math.min(c1, c2)
        }
      }
    }

    def isBigger(y: Array[Array[Int]]): Array[Array[Boolean]] = {
      array.zip(y).map {
        (r1, r2) => r1.zip(r2).map  {
          (c1, c2) => c1 > c2
        }
      }
    }

    def calculateScenicScore: Array[Array[Int]] = {
      array.zipWithIndex.map { (row, i) =>
        row.zipWithIndex.map { (cell, j) =>
          array.calculateScenicScoreAtPos(i, j)
        }
      }
    }

    def calculateScenicScoreAtPos(i: Int, j: Int): Int = {
      val row = array(i)
      val col = array.map(_(j))
      if (i == 0 || j == 0 || i == col.length - 1 || j == row.length - 1)
        0
      else {
        val value = array(i)(j)

        var left = 1
        var k = j - 1
        while (k > 0 && row(k) < value) {
          left += 1
          k -= 1
        }

        var top = 1
        k = i - 1
        while (k > 0 && col(k) < value) {
          top += 1
          k -= 1
        }

        var right = 1
        k = j + 1
        while (k < row.length - 1 && row(k) < value) {
          right += 1
          k += 1
        }

        var bottom = 1
        k = i + 1
        while (k < col.length - 1 && col(k) < value) {
          bottom += 1
          k += 1
        }

        left*right*bottom*top
      }
    }

  extension (array: Array[Array[Boolean]])
    def countTrue: Int = array.flatten.map { b => if (b) 1 else 0 }.sum

  val source = Source.fromFile("resources/input-day-08")
  val input = source.getLines.toArray
  source.close

  val matrix = input.map(_.toCharArray.toArray.map(_.asDigit))
  val maxFromLeft = matrix.map { row =>
    row.scanLeft(-1)(_.max(_)).dropRight(1)
  }
  val maxFromRight = matrix.map { row =>
    row.scanRight(-1)(_.max(_)).drop(1)
  }
  val maxFromTop = matrix.transpose.map { col =>
    col.scanLeft(-1)(_.max(_)).dropRight(1)
  }.transpose
  val maxFromBottom = matrix.transpose.map { col =>
    col.scanRight(-1)(_.max(_)).drop(1)
  }.transpose
  val minVals = maxFromLeft
    .combineMin(maxFromRight)
    .combineMin(maxFromBottom)
    .combineMin(maxFromTop)

  val visible = matrix.isBigger(minVals)
  println(s"Total number: ${visible.countTrue}")
  println(s"Max scenic number: ${matrix.calculateScenicScore.flatten.max}")
}

private def day07: Unit = {
  val source = Source.fromFile("resources/input-day-07")
  val input = source.getLines.toList
  source.close

  trait Path
  case class File(name: String, size: Int) extends Path
  case class Dir(path: String, var size: Int = 0) extends Path

  val fileMap = MutableMap[String, ListBuffer[Path]]()
  var pathStack = Stack[String]()
  input foreach { line =>
    val current = "/" + pathStack.reverse.mkString("/")
    line match {
      case "$ cd /" => pathStack.popAll
      case "$ cd .." => pathStack.pop
      case s"$$ cd ${path}" => pathStack.push(path)
      case "$ ls" => fileMap(current) = ListBuffer[Path]()
      case s"dir ${name}" => fileMap(current) += (current match {
        case "/" => Dir(s"/$name")
        case _ => Dir(s"$current/$name")
      })
      case s"${size} ${name}" => fileMap(current) += File(name, size.toInt)
      case _ => None
    }
  }

  def calc_size(files: ListBuffer[Path]): Int = {
    var sum = 0
    files foreach { x =>
      x match {
        case d: Dir => {
          d.size = calc_size(fileMap(d.path))
          sum += d.size
        }
        case f: File => sum += f.size
        case _ => None
      }
    }
    sum
  }

  val root = Dir("/", calc_size(fileMap("/")))

  val dirMap = MutableMap[String, Int]()
  dirMap("/") = root.size

  fileMap.values foreach { x =>
    x foreach { y =>
      y match {
        case d: Dir => dirMap(d.path) = d.size
        case _ => None
      }
    }
  }

  val sumOfSmallDirs = dirMap
    .filter { (key, value) => value <= 100000 }
    .values
    .sum
  println(s"Sum of small dirs: $sumOfSmallDirs")


  val totalSpace = 70000000
  val spaceRequired = 30000000

  val spaceAvailable = totalSpace - root.size
  val mustFreeUp = spaceRequired - spaceAvailable
  println(s"Must free up size: $mustFreeUp\n")

  val smallestButLargeEnough = dirMap
    .filter { (key, value) => value >= mustFreeUp }
    .toList
    .sortBy (_._2)
    .foreach { (k, v) => println(f"$v%10d $k")}
}

private def day06: Unit = {
  def identify(s: String, n: Int): Int = {
    (n to s.length).zipWithIndex
      .map { (i2, i1) => (i1, i2, s.substring(i1, i2).toSet.size) }
      .takeWhile { _._3 < n }
      .last
      ._2 + 1
  }

  val source = Source.fromFile("resources/input-day-06")
  val input = source.getLines.toList(0)
  source.close

  println(s"${identify("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4)} 7")
  println(s"${identify("bvwbjplbgvbhsrlpgdmjqwftvncz", 4)} 5")
  println(s"${identify("nppdvjthqldpwncqszvftbrmjlhg", 4)} 6")
  println(s"${identify("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4)} 10")
  println(s"${identify("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4)} 11")
  println(s"Answer: ${identify(input, 4)}")

  println(s"${identify("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14)} 19")
  println(s"${identify("bvwbjplbgvbhsrlpgdmjqwftvncz", 14)} 23")
  println(s"${identify("nppdvjthqldpwncqszvftbrmjlhg", 14)} 23")
  println(s"${identify("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14)} 29")
  println(s"${identify("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14)} 26")
  println(s"Answer: ${identify(input, 14)}")
}

private def day05: Unit = {
  def parse_input(filename: String): (
    List[Stack[Char]],
    List[(Int, Int, Int)]
  ) = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    source.close

    val (stack_lines_raw, procedure_lines) = lines.splitAt(lines.indexOf(""))

    // Number of stacks
    val n = stack_lines_raw.last.split(" ").last.toInt

    // Build the stacks
    val stack_lines = stack_lines_raw.reverse.tail
    val stacks = List range(0, n) map(1 + _*4) map { pos =>
      val stack = Stack[Char]()
      for (line <- stack_lines) {
        if (line.length >= pos) {
          val char = line.charAt(pos)
          if (char != ' ') stack.push(char)
        }
      }
      stack
    }

    val steps = procedure_lines.tail.map(_.split(" ") match {
      case Array(a, b, c, d, e, f) => (b.toInt, d.toInt, f.toInt)
      case _ => (0, 0, 0)
    })

    (stacks, steps)
  }

  val (stacks, steps) = parse_input("resources/input-day-05")

  // for ((move, from, to) <- steps) {
  //   for (_ <- 1 to move) {
  //     stacks(to-1).push(stacks(from-1).pop)
  //   }
  // }
  for ((move, from, to) <- steps) {
    val tmpstack = Stack[Char]()
    for (_ <- 1 to move) {
      tmpstack.push(stacks(from-1).pop)
    }
    stacks(to-1).pushAll(tmpstack)
  }

  val string = (for {
    x <- stacks
  } yield(x.top)).mkString ("")
  println(string)
}

private def day04: Unit = {
  def inputToSet(s: String): Set[Int] = s.split('-')
    .toList
    .map(_.toInt) match {
      case List(a, b) => Range(a, b).inclusive.toSet
      case _ => Set[Int]()
    }
  def overlapFull(sets: List[Set[Int]]) = sets match {
    case List(a, b) => (a subsetOf b) || (b subsetOf a)
    case _ => false
  }
  def overlap(sets: List[Set[Int]]) = sets match {
    case List(a, b) => !(a intersect b).isEmpty
    case _ => false
  }
  def sumBool(list: List[Boolean]): Int = list.map(if _ then 1 else 0).sum

  val filename =  "resources/input-day-04"
  val source = Source.fromFile(filename)
  val lines = source.getLines.toList
  source.close

  val inputSets = lines.map (_.split(',').toList.map (inputToSet))
  val sumFull = sumBool(inputSets.map(overlapFull))
  println(s"Number of fully contained pairs: $sumFull")

  val sumPartial = sumBool(inputSets.map(overlap))
  println(s"Number of partially contained pairs: $sumPartial")
}

private def day03: Unit = {
  def getDuplicate(s: String): Char = {
    val n = s.length()
    val s1 = s.substring(0, n/2).toSet
    val s2 = s.substring(n/2, n).toSet
    s1.intersect(s2).head
  }
  def getPriority(c: Char) = {
    c.getNumericValue - 9 + (if c.isUpper then 26 else 0)
  }
  val source = Source.fromFile("resources/input-day-03")
  val lines = source.getLines.toList
  // toVector er bedre
  source.close()
  val sum_priority = lines.map(getDuplicate).map(getPriority).sum
  val group_priority = lines
    .map(_.toSet)
    .grouped(3)
    .map { g => g.reduce(_ intersect _).head }
    .map(getPriority)
    .sum
  println(sum_priority)
  println(group_priority)
}

private def day02: Unit = {
  def play1(pair: Array[String]): Int = {
    val outcome = pair match {
      case Array("A", "X") => 3
      case Array("A", "Y") => 6
      case Array("A", "Z") => 0
      case Array("B", "X") => 0
      case Array("B", "Y") => 3
      case Array("B", "Z") => 6
      case Array("C", "X") => 6
      case Array("C", "Y") => 0
      case Array("C", "Z") => 3
      case _ => 0
    }
    val shape = pair(1) match {
      case "X" => 1
      case "Y" => 2
      case "Z" => 3
      case _ => 0
    }
    outcome + shape
  }

  def play2(pair: Array[String]): Int = pair match {
    case Array("A", "X") => 3
    case Array("A", "Y") => 4
    case Array("A", "Z") => 8
    case Array("B", "X") => 1
    case Array("B", "Y") => 5
    case Array("B", "Z") => 9
    case Array("C", "X") => 2
    case Array("C", "Y") => 6
    case Array("C", "Z") => 7
    case _ => 0
  }

  val source = Source.fromFile("resources/input-day-02")
  // val source = Source.fromFile("resources/input-day-02a")
  val pairs = source.getLines().map (_.split(" ") ).toList
  println(s"Sum 1: ${pairs.map(play1).sum}")
  println(s"Sum 2: ${pairs.map(play2).sum}")
  source.close()
}

private def day01: Unit = {
  val source = Source.fromFile("resources/input-day-01")
  val input = source.mkString
  source.close()

  val mylist = input
    .split("\n\n")
    .map( _.split("\n").map(_.toInt) )
    .zipWithIndex

  val x = mylist.map { (list, i) =>
    (i, list.length, list.sum)
  }
    .sortBy (_._3)(Ordering.Int.reverse)

  println(s"The elf is number ${x(0)._1} carrying ${x(0)._3}")

  val y = x.take(3).map(_._3).sum
  println(s"The top three elves carry: $y")
}
