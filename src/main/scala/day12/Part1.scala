package day12
import util.*

import java.io.File
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.math.Ordering

case class Point1(x: Int, y: Int):
    def translate(dx: Int, dy: Int) = Point1(x + dx, y + dy)

class Graph1(data: Array[Array[Char]], startPoint: Point1, endPoint: Point1):
    def width: Int = data(0).length
    def height: Int = data.length

    private val visited = Array.fill[Option[Point1]](height, width)(None)

    def seenFrom(point: Point1, prevPoint: Point1) = {
        visited(point.y)(point.x) = Some(prevPoint)
    }

    def length = {
        var k = visited(endPoint.y)(endPoint.x)
        var l = 0
        var steps = List(endPoint)
        while (k.nonEmpty) {
            steps = k.get :: steps
            k match {
                case Some(p) => k = visited(p.y)(p.x)
                case None    => k = None
            }
            l += 1
        }

        println(steps)
        l
    }

    def isSeen(point: Point1): Boolean = point == startPoint || visited(point.y)(point.x).nonEmpty

    def at(point: Point1): Char = data(point.y)(point.x)

    def contains(p: Point1) = p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

    def reachable(from: Point1, to: Point1) = {
        val fromC = at(from)
        val toC = at(to)

        toC - fromC <= 1
    }

    def possibleSteps(point: Point1) = {
        val candidates = Vector(
          point.translate(-1, 0),
          point.translate(0, -1),
          point.translate(1, 0),
          point.translate(0, 1)
        )

        candidates.filter(x => contains(x) && reachable(point, x) && !isSeen(x))
    }

@main def day12_part1() = {
    val sep = System.lineSeparator()

    var start = Point1(0, 0)
    var end = Point1(0, 0)

    val input = util
        .readInput(12)
        .split(sep)
        .map(_.toCharArray)

    input.zipWithIndex.foreach((row, y) => {
        val startLocationX = row.indexOf('S')
        val endLocationX = row.indexOf('E')

        if (startLocationX >= 0) {
            start = Point1(startLocationX, y)
        }

        if (endLocationX >= 0) {
            end = Point1(endLocationX, y)
        }
    })

    input(start.y)(start.x) = 'a'
    input(end.y)(end.x) = 'z'

    val graph = Graph1(input, start, end)

    val nodes = mutable.Queue(start)

    var done = false

    while (nodes.nonEmpty && !done) {
        val node = nodes.dequeue()
        println(node)

        val steps = graph.possibleSteps(node)
        steps.foreach(step => {
            graph.seenFrom(step, node)
        })

        done = steps.contains(end)
        nodes.addAll(steps)
    }

    println(graph.length)
//    graph.println(s"$width, $height")
}
