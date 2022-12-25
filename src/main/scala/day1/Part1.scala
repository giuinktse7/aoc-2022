package day1
import util.*
import java.io.File
import scala.math.Ordering

@main def day1_part1() = {
  val sep = System.lineSeparator()

  val input = util.readInput(1)

  val result =
    input
      .split(s"$sep$sep")
      .map(group => group.split(sep).foldLeft(0)((a, b) => a + b.toInt))
      .max

  println(result)
}
