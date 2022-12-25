package day25
import util.*
import java.io.File
import scala.math.Ordering

def snafu2ToDecimal(s: String) = {
    var result: Long = 0L

    val length = s.length

    var i = length - 1
    while (i >= 0) {
        val v = s(i) match {
            case '-' => -1
            case '=' => -2
            case c   => c.toString.toInt
        }

        result += math.pow(5, length - i - 1).toLong * v
        i -= 1
    }

    result
}

def log5(x: Long) = math.log(x) / math.log(5)

def decimalToSnafu(x: Long) = {
    def heuristic(node: Node, value: Long) = {
        (x - value).abs + math.pow(5, node.index).toLong
    }

    def neighbors(node: Node) = {
        if (node.index == 0) then Nil
        else Seq(-2, -1, 0, 1, 2).map(x => Node(node.index - 1, x)).toList
    }

    val nodes = AStar(
      start = Node(math.floor(log5(x)).toInt + 2, 0),
      goal = x,
      heuristic,
      neighbors
    )

    val nodesWithZeros = nodes ++ (0 until nodes.last.index).map(i => Node(i, 0))

    nodesWithZeros
        .dropWhile(_.value == 0)
        .map(x => x.value)
        .map {
            case -2 => '='
            case -1 => '-'
            case d  => d.toString.toCharArray()(0)
        }
        .mkString
}

@main def day25_part1() = {
    val sep = System.lineSeparator()

    val example = false
    val input = util.readInput(25, example)

    val values = input
        .split(sep)
        .map(snafu2ToDecimal)

    val sum = values.sum
    val result = decimalToSnafu(sum)

    println(s"Part 1:\n\tDecimal: $sum\n\tSnafu: $result")
}
