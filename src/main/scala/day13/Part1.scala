package day13
import day13.Status.{Failure, Success, Undecided}
import util.*
import scala.util.chaining._
import java.io.File
import scala.::
import scala.annotation.tailrec
import scala.math.Ordering
import scala.util.control.Breaks.{break, breakable}

@main def day13_part1() = {
    val sep = System.lineSeparator()

    val input = util
        .readInput(13)
        .split(s"$sep$sep")
        .map(x => x.split(sep))

     val comparisons: Seq[Status] = input
        .map(_.map(x => parse(Tokenizer(x))))
        .map { case Array(p1, p2) => compare(p1, p2) }

    val result = comparisons.zipWithIndex
        .filter((x, _) => x == Success)
        .map((_, i) => i + 1)
        .sum

    println(result)
}
