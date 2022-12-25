package day16
import util.*

import java.io.File
import scala.collection.mutable
import scala.math.{Ordering, min}

case class Valve(id: Int, name: String, flowRate: Int, neighbors: List[String]):
    var maskId: Int = -1

    def isOpen(mask: Int): Boolean = (mask & (1 << maskId)) != 0

@main def day16_part1() = {
    val sep = System.lineSeparator()

    val valveR = raw"Valve ([^ ]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)".r

    val input = util.readInput(16)

    var maskId = 0
    val valves: Array[Valve] = input
        .split(sep)
        .zipWithIndex
        .map { case (valveR(name, flowRate, connectedValves), i) =>
            val valve = Valve(i, name, flowRate.toInt, connectedValves.split(",").map(_.trim).toList)
            if (valve.flowRate > 0) {
                valve.maskId = maskId
                maskId += 1
            }

            valve
        }

    val valveMap = valves.map(v => (v.name, v)).toMap
    val shortestPath = Array.fill(valves.length, valves.length)(1e6.toInt)

    valves.zipWithIndex.foreach((valve, i) => {
        shortestPath(i)(i) = 0
        valve.neighbors.foreach { valveName => shortestPath(i)(valveMap(valveName).id) = 1 }
    })

    // Floyd–Warshall for shortest paths
    for
        ids <- Some(valves.map(_.id))
        hop <- ids
        i <- ids
        j <- ids
    do shortestPath(i)(j) = math.min(shortestPath(i)(j), shortestPath(i)(hop) + shortestPath(hop)(j))

    val valvesWithFlow = valves.filter(_.flowRate > 0)

    case class Node(openedMask: Int, remainingTime: Int, valveName: String, pressure: Int)

    def searchGraph(t: Int): List[Node] = {
        var result: List[Node] = Nil
        val nodes = mutable.Stack[Node]()
        val visited = mutable.Set[Int]()

        nodes.push(Node(0, t, "AA", 0))
        while (nodes.nonEmpty) {
            val node = nodes.pop()
            val key = node.hashCode()

            if (!visited.contains(key)) {
                visited.add(key)
                result = node :: result

                if (node.remainingTime > 0) {
                    val valveId = valveMap(node.valveName).id
                    valvesWithFlow.filterNot(_.isOpen(node.openedMask)).foreach { valve =>
                        val t1 = node.remainingTime - shortestPath(valveId)(valve.id) - 1
                        if (t1 > 0) {
                            val opened = node.openedMask | (1 << valve.maskId)
                            nodes.push(Node(opened, t1, valve.name, node.pressure + t1 * valve.flowRate))
                        }
                    }
                }
            }
        }

        result
    }

    // Part 1
    val answer1 = searchGraph(30).foldLeft(0)((a, b) => math.max(a, b.pressure))
    println(s"Answer1: $answer1");

    // Part 2
    val finalPressure = searchGraph(26).foldLeft(mutable.Map[Int, Int]())((a, node) => {
        a.updateWith(node.openedMask) {
            case None           => Some(node.pressure)
            case Some(pressure) => Some(math.max(pressure, node.pressure))
        }

        a
    })

    val pressures = for
        (mask1, pressure1) <- finalPressure
        (mask2, pressure2) <- finalPressure
        overlap = mask1 & mask2 if overlap == 0
    yield pressure1 + pressure2

    println(s"Answer 2: ${pressures.max}")
}
