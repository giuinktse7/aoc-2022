package day12
import util.*

import java.io.File
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.math.Ordering
import scala.util.control.NonLocalReturns

case class Point(x: Int, y: Int):
    def +(d: (Int, Int)) = Point(x + d._1, y + d._2)

class Graph(data: Array[Array[Char]]):
    def width: Int = data(0).length
    def height: Int = data.length

    def reachablePoints(point: Point): Seq[Point] =
        deltas.map(d => point + d).filter(x => contains(x) && reachable(point, x))

    private val deltas = Vector((-1, 0), (0, -1), (1, 0), (0, 1))

    private def at(point: Point): Char = data(point.y)(point.x)
    private def contains(p: Point): Boolean = p.x >= 0 && p.x < width && p.y >= 0 && p.y < height
    private def reachable(from: Point, to: Point): Boolean = at(to) - at(from) <= 1

def shortestPath(input: Array[Array[Char]], start: Point, end: Point): Option[Int] = {
    val graph = Graph(input)
    val points = mutable.Queue(start)

    val previous = Array.fill[Option[Point]](graph.height, graph.width)(None)

    @tailrec
    def pathLength(point: Point, length: Int = 0): Int =
        previous(point.y)(point.x) match {
            case Some(p) => pathLength(p, length + 1)
            case None    => length
        }

    var done = false
    while (points.nonEmpty && !done) {
        val point = points.dequeue()
        val newPoints = graph.reachablePoints(point).filter(p => previous(p.y)(p.x).isEmpty && p != start)

        for (p <- newPoints) {
            previous(p.y)(p.x) = Some(point)
            if (p == end) {
                done = true
            }
        }

        points.addAll(newPoints)
    }

    if done then Some(pathLength(end)) else None
}

@main def day12_part2(): Unit = {
    val sep = System.lineSeparator()
    val input = util.readInput(12)

    val data = input.split(sep).map(_.toCharArray)

    val width = data(0).length

    def findCharPos(char: Char) =
        data.flatten.zipWithIndex
            .find((c, _) => c == char)
            .map((_, i) => Point(i % width, i / width))
            .get

    val start = findCharPos('S')
    val end = findCharPos('E')

    data(start.y)(start.x) = 'a'
    data(end.y)(end.x) = 'z'

    val starts =
        data.zipWithIndex.flatMap((line, y) => line.zipWithIndex.filter((c, _) => c == 'a').map((_, x) => Point(x, y)))

    // Part 1
    println("Part 1: " + starts.take(1).flatMap(start => shortestPath(data, start, end)).min)

    // Part 2
    println("Part 2: " + starts.flatMap(start => shortestPath(data, start, end)).min)
}
