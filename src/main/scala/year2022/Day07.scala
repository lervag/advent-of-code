package year2022

import scala.collection.mutable.{Stack, Map => MutableMap, ListBuffer}
import scala.io.Source

sealed trait Path
sealed case class File(name: String, size: Int) extends Path
sealed case class Dir(path: String, var size: Int = 0) extends Path

def day07: Unit = {
  val source = Source.fromFile("resources/input-day-07")
  val input = source.getLines.toList
  source.close

  val fileMap = MutableMap[String, ListBuffer[Path]]()
  var pathStack = Stack[String]()
  input foreach { line =>
    val current = "/" + pathStack.reverse.mkString("/")
    line match {
      case "$ cd /"         => pathStack.popAll
      case "$ cd .."        => pathStack.pop
      case s"$$ cd ${path}" => pathStack.push(path)
      case "$ ls"           => fileMap(current) = ListBuffer[Path]()
      case s"dir ${name}" =>
        fileMap(current) += (current match {
          case "/" => Dir(s"/$name")
          case _   => Dir(s"$current/$name")
        })
      case s"${size} ${name}" => fileMap(current) += File(name, size.toInt)
      case _                  => None
    }
  }

  val root = Dir("/", calc_size(fileMap("/"), fileMap))

  val dirMap = MutableMap[String, Int]()
  dirMap("/") = root.size

  fileMap.values foreach { x =>
    x foreach { y =>
      y match {
        case d: Dir => dirMap(d.path) = d.size
        case _      => None
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
    .sortBy(_._2)
    .foreach { (k, v) => println(f"$v%10d $k") }
}

private def calc_size(
    files: ListBuffer[Path],
    fileMap: MutableMap[String, ListBuffer[Path]]
): Int = {
  var sum = 0
  files foreach { x =>
    x match {
      case d: Dir => {
        d.size = calc_size(fileMap(d.path), fileMap)
        sum += d.size
      }
      case f: File => sum += f.size
      case null    => None
    }
  }
  sum
}
