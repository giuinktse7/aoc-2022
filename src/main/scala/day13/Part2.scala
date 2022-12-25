package day13
import day13.Status.{Failure, Success, Undecided}
import util.*

import java.io.File
import scala.::
import scala.annotation.tailrec
import scala.math.Ordering
import scala.util.control.Breaks.{break, breakable}

@main def day13_part2() = {
    val sep = System.lineSeparator()

    val input = "[[2]]" :: "[[6]]" :: util
        .readInput(13)
        .split(s"$sep")
        .filterNot(_.isBlank)
        .toList

    val result = input.zipWithIndex
        .map((x, id) => (id, parse(Tokenizer(x))))
        .sortWith { case ((_, p1), (_, p2)) => compare(p1, p2) != Failure }
        .zipWithIndex
        .filter { case ((id, _), _) => id <= 1 }
        .map { case (_, i) => i + 1 }
        .product

    println(result)
}
