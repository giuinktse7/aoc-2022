package day6
import util.*
import java.io.File
import scala.math.Ordering

@main def day6_part1() = {
    val input = util.readInput(6)

    val windowSize = 4

    val result = input
        .sliding(windowSize)
        .zipWithIndex
        .find((window, _) => window == window.distinct)
        .map((_, i) => i + windowSize)
        .get

    println(result)
}
