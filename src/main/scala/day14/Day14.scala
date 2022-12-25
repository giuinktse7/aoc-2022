package day14
import util.*

import java.io.File
import scala.math.{Ordering, log}
import Tile.*

import scala.collection.mutable

enum Tile:
    case Void, Sand, Rock

case class Pos(x: Int, y: Int):
    def +(delta: (Int, Int)): Pos = Pos(x + delta._1, y + delta._2)

case class Map(topLeft: Pos, bottomRight: Pos):
    private val defaultPadX = (bottomRight.x - topLeft.x) * math.ceil(math.log(bottomRight.y - topLeft.y)).toInt
    private var padY = topLeft.y
    private val padX = mutable.Map[Int, Int]().withDefaultValue(defaultPadX / 2)

    private var data: Array[Array[Tile]] =
        Array.fill(bottomRight.y - topLeft.y + padY + 2, bottomRight.x - topLeft.x + defaultPadX)(Void)

    private var xMin = topLeft.x
    private var xMax = bottomRight.y

    private var yMin = topLeft.y
    private var yMax = bottomRight.y

    def width: Int = data(0).length
    def height: Int = data.length

    def abyssDepth: Int = bottomRight.y + 1

    private def kernel(pos: Pos): Pos =
        val y = pos.y - topLeft.y + padY
        Pos(pos.x - topLeft.x + padX(pos.y), y)

    def set(pos: Pos, tile: Tile): Unit = {
        var p = kernel(pos)

        // Handle growth in Y
        if (p.y >= data.length) {
            data = data ++ Array.fill(height / 2, data.last.length)(Void)
        } else if (p.y < 0) {
            val count = height / 2
            data = Array.fill(height / 2, data.head.length)(Void) ++ data
            padY += count
            p = kernel(pos)
        }

        // Handle growth in X
        if (p.x < 0 || p.x >= data(p.y).length) {
            val count = data(p.y).length / 4
            val part = () => Array.fill(count)(Void)

            // Grow equally in both directions
            data(p.y) = part() ++ data(p.y) ++ part()

            padX(pos.y) += count
            p = kernel(pos)
        }

        data(p.y)(p.x) = tile

        xMin = math.min(pos.x, xMin)
        xMax = math.max(pos.x, xMax)

        yMin = math.min(pos.y, yMin)
        yMax = math.max(pos.y, yMax)
    }

    def at(pos: Pos): Tile = {
        val p = kernel(pos)
        data(p.y)(p.x)
    }

    def isVoid(pos: Pos): Boolean = !contains(pos) || at(pos) == Void

    def contains(pos: Pos): Boolean =
        val p = kernel(pos)
        p.x >= 0 && p.y >= 0 && p.y < height && p.x < data(p.y).length

    def draw: String = {
        val (xPad, yPad) = (1, 3)

        val seq = for
            y <- yMin - yPad to yMax + yPad
            x <- xMin - xPad to xMax + xPad
            pos = Pos(x, y)
        yield {
            if contains(pos) then
                at(pos) match {
                    case Void => '.'
                    case Rock => '#'
                    case Sand => 'o'
                }
            else '.'
        }

        val groupSize = (xMax - xMin) + xPad * 2 + 1
        seq.grouped(groupSize).map(_.mkString).mkString("\n")
    }

def dropSand1(map: Map, source: Pos): Boolean = {
    var pos = source

    while (pos.y < map.abyssDepth) {
        val next: Option[Pos] = Vector((0, 1), (-1, 1), (1, 1)).map(x => pos + x).find(x => map.isVoid(x))
        next match {
            case Some(p) => pos = p
            case None =>
                map.set(pos, Sand)
                return true
        }
    }

    false
}

def dropSand2(map: Map, source: Pos): Boolean = {
    var pos = source

    while (pos.y < map.abyssDepth) {
        val next: Option[Pos] = Vector((0, 1), (-1, 1), (1, 1)).map(x => pos + x).find(x => map.isVoid(x))
        next match {
            case Some(p) => pos = p
            case None =>
                map.set(pos, Sand)
                return pos != Pos(500, 0)

        }
    }

    map.set(pos, Sand)
    true
}

def createMap(input: String) = {
    val sep = System.lineSeparator()

    val posString = raw"(\d+),(\d+)".r

    val rockStructures = input
        .split(sep)
        .map(_.split("->").map(_.trim).map { case posString(x, y) => Pos(x.toInt, y.toInt) })

    val positions = rockStructures.flatten

    val topLeft = Pos(positions.map(_.x).min, positions.map(_.y).min)
    val bottomRight = Pos(positions.map(_.x).max, positions.map(_.y).max)

    val map = Map(topLeft, bottomRight)

    for
        rock <- rockStructures
        (from, to) <- rock.zip(rock.drop(1))

        x0 = math.min(from.x, to.x)
        x1 = math.max(from.x, to.x)

        y0 = math.min(from.y, to.y)
        y1 = math.max(from.y, to.y)

        x <- x0 to x1
        y <- y0 to y1
        pos = Pos(x, y)
    do map.set(pos, Rock)

    map
}

@main def day14() = {
    val input = util.readInput(14)

    val sourcePos = Pos(500, 0)

    {
        val map = createMap(input)
        val result = infinite(() => dropSand1(map, sourcePos)).takeWhile(_()).size
        println(map.draw)
        println(s"Part 1: $result")
    }

    {
        val map = createMap(input)
        val result = infinite(() => dropSand2(map, sourcePos)).takeWhile(_()).size + 1
        println(map.draw)
        println(s"Part 2: $result")
    }

}
