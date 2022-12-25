package day4
import util.*

import java.io.File
import scala.math.{Ordering, signum}

@main def day4_part2(): Unit = {
  val sep = System.lineSeparator()

  val input = util.readInput(4)
  val assignmentPair = raw"(\d+)-(\d+),(\d+)-(\d+)".r

  val result = input
    .split(sep)
    .count {
      case assignmentPair(l1, h1, l2, h2) => h1.toInt >= l2.toInt && h2.toInt >= l1.toInt
    }

  println(result)
}
