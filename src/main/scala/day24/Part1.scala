package day24
import util.*
import java.io.File
import scala.math.Ordering
import scala.collection.mutable

case class Pos(x: Int, y: Int):
    def +(delta: (Int, Int)) = Pos(x + delta._1, y + delta._2)
    def manhattan(that: Pos) = (this.x - that.x).abs + (this.y - that.y).abs

case class Node(pos: Pos, t: Int)

case class Storm(i: Int, forward: Boolean)

@main def day24_part1() = {
    val sep = System.lineSeparator()

    val example = false
    val input = util.readInput(24, example)
    val rows = input.split(sep)

    val first = rows.head
    val last = rows.last

    val width = first.length
    val height = rows.length

    println(s"width: $width, height: $height")

    val start = Pos(first.indexOf('.'), 0)
    val end = Pos(last.indexOf('.'), height - 1)

    var timeOffset = 0
    val blockedCache = mutable.Set[(Pos, Int)]()

    val (horizontalStorms, verticalStorms) =
        rows.zipWithIndex.foldLeft((mutable.Map[Int, List[Storm]](), mutable.Map[Int, List[Storm]]())) {
            case ((hs, vs), (row, y)) =>
                def prepend(m: mutable.Map[Int, List[Storm]], i: Int, value: Storm) =
                    m.updateWith(i)(curr => Some(value :: curr.getOrElse(Nil)))

                row.zipWithIndex.foreach { (c, x) =>
                    c match {
                        case 'v' => prepend(vs, x, Storm(y, forward = true))
                        case '^' => prepend(vs, x, Storm(y, forward = false))
                        case '<' => prepend(hs, y, Storm(x, forward = false))
                        case '>' => prepend(hs, y, Storm(x, forward = true))
                        case _   =>

                    }
                }

                (hs, vs)
        }

    def getStormPos(size: Int, storm: Storm, steps: Int) = {
        val delta = steps + timeOffset
        if (storm.forward) {
            ((storm.i - 1 + delta) % size) + 1
        } else {
            val result = (storm.i - delta - 1) % size
            (if result < 0 then result + size else result) + 1
        }
    }

    var hits = 0
    var misses = 0

    def blocked(pos: Pos, t: Int): Boolean = {
        if (blockedCache.contains((pos, t))) {
            hits += 1
            return true
        }

        val b1 = horizontalStorms.get(pos.y) match {
            case None         => Nil
            case Some(storms) => storms.map(s => getStormPos(width - 2, s, t))
        }

        blockedCache.addAll(b1.map(x => (Pos(x, pos.y), t)))

        if (b1.contains(pos.x)) {
            misses += 1
            return true
        }

        val b2 = verticalStorms.get(pos.x) match {
            case None         => Nil
            case Some(storms) => storms.map(s => getStormPos(height - 2, s, t))
        }

        blockedCache.addAll(b2.map(y => (Pos(pos.x, y), t)))

        if (b2.contains(pos.y)) {
            misses += 1
            return true
        }

        false
    }

    def neighborPositions(pos: Pos) = {
        List(
          Pos(pos.x - 1, pos.y),
          Pos(pos.x + 1, pos.y),
          Pos(pos.x, pos.y - 1),
          Pos(pos.x, pos.y + 1),
          Pos(pos.x, pos.y)
        )
    }

    def neighbors(node: Node, t: Int): List[Node] = {
        val t2 = t + 1

        neighborPositions(node.pos)
            .filter {
                case pos if pos == end => true
                case pos =>
                    val insideMap = (0 < pos.x && pos.x < width - 1 && 0 < pos.y && pos.y < height - 1) || pos == start
                    insideMap && !blocked(pos, t2)
            }
            .map {
                case pos if pos == node.pos => node.copy(t = t2)
                case pos                    => Node(pos, t2)
            }
    }

    def horizontalStormAt(pos: Pos, t: Int): Option[Char] = {
        horizontalStorms.get(pos.y).flatMap { storms =>
            storms.find(s => getStormPos(width - 2, s, t) == pos.x).map(s => if s.forward then '>' else '<')
        }
    }
    def verticalStormAt(pos: Pos, t: Int): Option[Char] = {
        verticalStorms.get(pos.x).flatMap { storms =>
            storms.find(s => getStormPos(height - 2, s, t) == pos.y).map(s => if s.forward then 'v' else '^')
        }
    }

    def drawMap(playerPos: Pos, t: Int) = {
        println(s"t: $t")
        val data = for
            y <- 0 until height
            x <- 0 until width
            p = Pos(x, y)
            hstorm = horizontalStormAt(p, t)
            vstorm = verticalStormAt(p, t)
            insideMap = 0 < x && x < width - 1 && 0 < y && y < height - 1
        yield {
            (hstorm, vstorm) match {
                case (Some(_), Some(_)) => '2'
                case (Some(s), _)       => s
                case (_, Some(s))       => s
                case _ =>
                    if p == playerPos then 'E'
                    else if insideMap || p == start || p == end then '.'
                    else '#'
            }
        }

        println(data.grouped(width).map(_.mkString).mkString("\n"))
    }

    def runAStar(startPos: Pos, endPos: Pos) =
        AStar(startPos, endPos, n => end.manhattan(n.pos) + n.t, neighbors, drawMap)

    val (r1, t1) = time { runAStar(start, end) }
    timeOffset += t1

    blockedCache.clear()

    val (_, t2) = time { runAStar(end, start) }
    timeOffset += t2

    blockedCache.clear()

    val (_, t3) = time { runAStar(start, end) }
    timeOffset += t3

    println(s"Time: $t1 + $t2 + $t3 = $timeOffset")
    println(s"Part 1: ${r1.size - 1}")
    println(s"Part 2: $timeOffset")

    println(s"(Cache) hits: $hits, misses: $misses")
}
