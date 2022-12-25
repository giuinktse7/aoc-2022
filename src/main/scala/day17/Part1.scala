package day17
import day17.Shape.{Horizontal, InvertedL, Plus, Vertical, Square}
import util.*

import java.io.File
import scala.math.Ordering

enum Shape:
    case Horizontal, Plus, InvertedL, Vertical, Square

case class Pos(x: Int, y: Int):
    def +(delta: (Int, Int)): Pos = Pos(x + delta._1, y + delta._2)
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)

def height(shape: Shape) = {
    shape match {
        case Horizontal       => 1
        case Square           => 2
        case Plus | InvertedL => 3
        case Vertical         => 4
    }
}

def width(shape: Shape) = {
    shape match {
        case Vertical         => 1
        case Square           => 2
        case Plus | InvertedL => 3
        case Horizontal       => 4
    }
}

def positions(shape: Shape, pos: Pos): Seq[Pos] = {
    val xs = shape match {
        case Horizontal => (0 to 3).map(dx => Pos(dx, 0))
        case Vertical   => (0 to 3).map(dy => Pos(0, -dy))
        case Plus       => Seq(Pos(1, 0), Pos(1, -1), Pos(1, -2), Pos(0, -1), Pos(2, -1))

        case Square    => Seq(Pos(0, 0), Pos(0, -1), Pos(1, 0), Pos(1, -1))
        case InvertedL => Seq(Pos(2, 0), Pos(2, -1), Pos(2, -2), Pos(1, -2), Pos(0, -2))
    }

    xs.map(_ + pos)
}
def positions(rock: Rock): Seq[Pos] = positions(rock.shape, rock.pos)

case class Rock(shape: Shape, var pos: Pos)

case class Map(xDropPos: Int, yDropOffset: Int):
    val data: Array[Array[Boolean]] = Array.fill(1000000, 7)(false)

    var maxY: Int = 0

    def nextRockPos(shape: Shape): Pos = Pos(xDropPos, maxY + height(shape) + yDropOffset)

    def occupied(pos: Pos): Boolean = data(pos.y - 1)(pos.x - 1)
    def occupied(x: Int, y: Int): Boolean = data(y - 1)(x - 1)

    def fits(shape: Shape, pos: Pos) =
        !positions(shape, pos).exists(pos => pos.x < 1 || pos.x > 7 || pos.y < 1 || occupied(pos))

    def fits(rock: Rock) = !positions(rock).exists(occupied)
    def tryPlace(rock: Rock): Boolean = {
        val xs = positions(rock)
        val ok = !xs.exists(occupied)

        if (ok) {
            xs.foreach { pos =>
                data(pos.y - 1)(pos.x - 1) = true
                maxY = math.max(maxY, pos.y)
            }
        }

        ok
    }

    def place(rock: Rock): Unit = {
        positions(rock).foreach { pos =>
            data(pos.y - 1)(pos.x - 1) = true
            maxY = math.max(maxY, pos.y)

        }
    }

    def fullRow(y: Int = maxY): Boolean = (1 to 7).map(x => Pos(x, y)).forall(occupied)

    def show: String = {
        val rows = for
            y <- 1 to maxY
            row = (1 to 7).map(x => if occupied(x, y) then '#' else '.').mkString
        yield row

        rows.reverse.mkString("\n")
    }

def part1(input: String) = {
    val jets = infiniteList(input.toCharArray.zipWithIndex).iterator
    val shapes = infiniteList(Seq(Horizontal, Plus, InvertedL, Vertical, Square)).iterator

    val map = Map(xDropPos = 3, yDropOffset = 3)

    for
        i <- 1 to 1000000
        shape = shapes.next()
    do {
        var prevPos = map.nextRockPos(shape)
        var pos = prevPos

        var ji = 0;

        while (map.fits(shape, pos)) {
            // Push with jet
            val (jet, jetIndex) = jets.next()
            ji = jetIndex
            val maybeDx = jet match {
                case '<' => if pos.x > 1 then Some(-1) else None
                case '>' => if pos.x <= 7 - width(shape) then Some(1) else None
            }

            for
                dx <- maybeDx
                p = pos + Pos(dx, 0) if map.fits(shape, p)
            do pos = p

            prevPos = pos

            // Fall
            pos = pos + Pos(0, -1)
        }

        map.place(Rock(shape, prevPos))

        if (map.fullRow()) {
            println(s"full at $i, jetIndex: $ji")
        }

    }

    println(map.maxY)
//    println(map.show)
}

def part2(input: String) = {
    val jets = infiniteList(input.toCharArray.zipWithIndex).iterator
    val shapes = infiniteList(Seq(Horizontal, Plus, InvertedL, Vertical, Square)).iterator

    val map = Map(xDropPos = 3, yDropOffset = 3)

    var h1: Long = 0
    var h2: Long = 0
    var h3: Long = 0
    var extraHeight: Long = 0
    var terminateIndex = Long.MaxValue

    var fullRowItems: List[(Long, Long)] = Nil

    for
        i <- 1 to 10000
        shape = shapes.next() if i <= terminateIndex
    do {
        var prevPos = map.nextRockPos(shape)
        var pos = prevPos

        var ji = 0;

        while (map.fits(shape, pos)) {
            // Push with jet
            val (jet, jetIndex) = jets.next()
            ji = jetIndex
            val maybeDx = jet match {
                case '<' => if pos.x > 1 then Some(-1) else None
                case '>' => if pos.x <= 7 - width(shape) then Some(1) else None
            }

            for
                dx <- maybeDx
                p = pos + Pos(dx, 0) if map.fits(shape, p)
            do pos = p

            prevPos = pos

            // Fall
            pos = pos + Pos(0, -1)
        }

        map.place(Rock(shape, prevPos))

        if (map.fullRow()) {
            println(s"full at $i, jetIndex: $ji")
            fullRowItems = fullRowItems :+ (i, map.maxY)
            println(fullRowItems)
            fullRowItems match {
                case (i1, h1) :: (i2, h2) :: Nil =>
                    val loopSize = i2 - i1
                    val remainingRocks = (1000000000000L - i)
                    extraHeight = (h2 - h1) * math.floor(remainingRocks / loopSize).toLong
                    terminateIndex = i + (remainingRocks % loopSize)
                    println(s"Terminate at $terminateIndex")
                case _ =>
            }
        }
        // Go until row 1741, record height h1
        // Go for 1715 more, get height as h = h_m - h1
        // h2 = h * math.floor((1000000000000 - i) / 1715)
        // Go for 15 more
        // Answer = h_m + h2
    }

    //    println(map.show)
    println(map.maxY)
    println(map.maxY + extraHeight)
    /*
    The tall, vertical chamber is exactly seven units wide.
    Each rock appears so that:
        Its left edge is two units away from the left wall
        Its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).
     */

    /*
    After a rock appears, it alternates between:
        Being pushed by a jet of hot gas one unit (in the direction indicated by the next symbol in the jet pattern)
        Falling one unit down.

    If any movement would cause any part of the rock to move into the walls, floor, or a stopped rock, the movement
    instead does not occur.

    If a downward movement would have caused a falling rock to move into the floor or an already-fallen rock,
    the falling rock stops where it is (having landed on something) and
    a new rock immediately begins falling.
     */
}

@main def day17_main() = {
//    val input = util.readInput(17, example = true)
    val input = util.readInput(17)
//    part1(input)
    
    // 1560932952143 : Too high
    part2(input)
}
