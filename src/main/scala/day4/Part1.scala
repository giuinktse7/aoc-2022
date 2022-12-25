package day4
import util.*

import java.io.File
import scala.math.{Ordering, signum}

@main def day4_part1() = {
  val sep = System.lineSeparator()

  val input = util.readInput(4)
  val assignmentPair = raw"(\d+)-(\d+),(\d+)-(\d+)".r

  val result = input
    .split(sep)
    .count {
      case assignmentPair(l1, h1, l2, h2) =>
        val s1 = signum(l1.toInt - l2.toInt)
        val s2 = signum(h1.toInt - h2.toInt)

        s1 != s2 || s1 == 0 && s2 == 0
    }

  println(result)
}
