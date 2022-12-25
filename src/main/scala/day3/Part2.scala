package day3
import util.*

import java.io.File
import scala.compiletime.ops.int.+
import scala.math.Ordering

@main def day3_part2(): Unit = {
  val sep = System.lineSeparator()

  val priority = (c: Char) => c.toInt match {
    case x if 'a'.toInt <= x && x <= 'z'.toInt => x - 'a'.toInt + 1
    case x => x - 'A'.toInt + 27
  }

  val input = util.readInput(3)

  val result = input.split(sep)
    .grouped(3)
    .map(x => x.map(_.toSet).reduce((x, y) => x.intersect(y)).map(priority).sum)
    .sum

  println(result)
}