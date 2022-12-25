package day18
import util.*
import java.io.File
import scala.math.Ordering
import scala.collection.mutable

case class Pos(x: Int, y: Int, z: Int):
    def +(delta: (Int, Int, Int)): Pos = Pos(x + delta._1, y + delta._2, z + delta._3)
    def -(delta: (Int, Int, Int)): Pos = Pos(x - delta._1, y - delta._2, z - delta._3)
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y, z + that.z)

case class Node(pos: Pos, face: (Int, Int, Int))

def cubeFaceInfo(face: (Int, Int, Int)) = face match {
    case (fx, 0, 0) => (fx, (self: Int, a: Int, b: Int) => (self, a, b))
    case (0, fy, 0) => (fy, (self: Int, a: Int, b: Int) => (a, self, b))
    case (0, 0, fz) => (fz, (self: Int, a: Int, b: Int) => (a, b, self))
}

class Grid(val width: Int, height: Int, depth: Int):
    val data: Array[Boolean] = Array.fill((width + 1) * (height + 1) * (depth + 1))(false)
    private val adjacentPositions: List[(Int, Int, Int)] = signedPermutations(Seq(1, 0, 0))
        .map { case Seq(x, y, z) => (x, y, z) }

    private val deltas = signedPermutations(0 to 1)

    private def index(pos: Pos): Int = width * (height * pos.z + pos.y) + pos.x
    private def within(pos: Pos): Boolean =
        pos.x >= 0 && pos.y >= 0 && pos.z >= 0 && pos.x <= width && pos.y <= height && pos.z <= depth

    private def hasCube(pos: Pos) = within(pos) && data(index(pos))

    def add(pos: Pos): Unit = data(index(pos)) = true

    def adjacent(pos: Pos): Seq[Pos] = adjacentPositions.map(x => pos + x).filter(hasCube)

    def reachableNodes(from: Pos, cubeFace: (Int, Int, Int)): List[Node] = {
        val (a1, f) = cubeFaceInfo(cubeFace)

        val values = deltas.map { case Seq(a2, a3) =>
            val diagonal = from + f(a1, a2, a3)

            if hasCube(diagonal) then Node(diagonal, f(0, -a2, -a3))
            else {
                val f1 = f(0, a2, a3)
                if hasCube(from + f1) then Node(from + f1, cubeFace) else Node(from, f1)
            }
        }

        values.filter(node => hasCube(node.pos))
    }

@main def day18_main(): Unit = {
    val sep = System.lineSeparator()

    val input = util.readInput(18)

    val positions = input
        .split(sep)
        .map(_.split(",") match {
            case Array(x, y, z) => Pos(x.toInt, y.toInt, z.toInt)
        })
        .toList

    val (width, height, depth) = positions.foldLeft((0, 0, 0)) { case ((w, h, d), pos) =>
        (w max pos.x, h max pos.y, d max pos.z)
    }

    val grid = Grid(width + 1, height + 1, depth + 1)
    positions.foreach(grid.add)

    day18_part1(grid, positions)
    day18_part2(grid, positions)
}

def day18_part1(grid: Grid, positions: Seq[Pos]): Unit = {
    val result = positions.map(x => 6 - grid.adjacent(x).size).sum
    println(s"Part 1: $result")
}

def day18_part2(grid: Grid, positions: Seq[Pos]): Unit = {
    val start = positions.filter(pos => pos.x == grid.width - 1).sortBy(pos => pos.y).reverse.maxBy(pos => pos.z)

    val visited = mutable.Set[Node]()
    val q = mutable.Queue[Node]()
    q.addOne(Node(start, (1, 0, 0)))

    while (q.nonEmpty) {
        val node = q.dequeue()

        if (!visited.contains(node)) {
            visited.add(node)
            q.addAll(grid.reachableNodes(node.pos, node.face).filterNot(visited.contains))
        }
    }

    val result = visited.size
    println(s"Part 2: $result")
}
