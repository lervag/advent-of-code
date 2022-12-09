import scala.util.{Using, Success, Failure}
import scala.collection.mutable.{Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

@main def main: Unit = day08

private def day08: Unit = {
  val source = Source.fromFile("resources/input-day-08a")
  val input = source.getLines.toArray
  source.close

  val n = input.size
  val m = input(0).length

  val matrix = input.map(_.toCharArray.toArray.map(_.asDigit))
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
