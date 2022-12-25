package day23
import util.*

import java.io.File
import scala.math.Ordering
import Direction.{South, *}
import scala.collection.mutable

case class Pos(x: Int, y: Int):
    def +(p: (Int, Int)) = Pos(x + p._1, y + p._2)

    def move(direction: Direction) = direction match {
        case North => Pos(x, y - 1)
        case South => Pos(x, y + 1)
        case West  => Pos(x - 1, y)
        case East  => Pos(x + 1, y)
    }

case class Elf(var pos: Pos)

enum Direction:
    case North, South, West, East

class Grove(width: Int, height: Int):
    private val offsetX = math.floor(width / 2).toInt
    private val offsetY = math.floor(height / 2).toInt
    private val data = Array.fill[Option[Elf]](height + 2, width + 2)(None)

    private var minX = width
    private var minY = height

    private var maxX = 0
    private var maxY = 0

    private def t(pos: Pos) = pos + (offsetX, offsetY)

    def at(pos: Pos) = {
        val Pos(x, y) = pos + (offsetX, offsetY)
        data(y)(x)
    }

    def within(pos: Pos) = {
        val Pos(x, y) = t(pos)
        0 <= x && x < width && 0 <= y && y < height
    }

    def move(elf: Elf, pos: Pos) = {
        assert(at(elf.pos).contains(elf))
        assert(at(pos).isEmpty)

        val Pos(x1, y1) = t(elf.pos)
        data(y1)(x1) = None
        set(pos, elf)
        elf.pos = pos
    }

    def at(x: Int, y: Int) = data(y + offsetY)(x + offsetX)

    def set(pos: Pos, elf: Elf) = {
        val Pos(x, y) = pos + (offsetX, offsetY)
        data(y)(x) = Some(elf)

        minX = math.min(pos.x, minX)
        minY = math.min(pos.y, minY)

        maxX = math.max(pos.x, maxX)
        maxY = math.max(pos.y, maxY)
    }

    def adjacent(pos: Pos) = {
        val candidates = for
            dx <- -1 to 1
            dy <- -1 to 1 if !(dx == 0 && dy == 0)
        yield (dx, dy)

        candidates
            .map { pos + _ }
            .filter(within)
            .flatMap(at)
            .toList
    }

    def emptyTiles = {
        var x1 = width
        var x2 = 0

        var y1 = height
        var y2 = 0

        for
            x <- minX to maxX
            y <- minY to maxY if at(x, y).nonEmpty
        do {
            x1 = math.min(x1, x)
            y1 = math.min(y1, y)

            x2 = math.max(x2, x)
            y2 = math.max(y2, y)
        }

        val empty = for
            x <- x1 to x2
            y <- y1 to y2 if at(x, y).isEmpty
        yield 1

        empty.size
    }

    def draw: String = {
        val pad = 2
        val data = for
            y <- (minY - pad) to (maxY + pad)
            x <- (minX - pad) to (maxX + pad)
        yield at(x, y) match {
            case _ if x == 0 && y == 0 => "0"
            case Some(_)               => "#"
            case None                  => "."
        }

        data.grouped(maxX - minX + 1 + pad * 2).map(_.mkString).mkString("\n")
    }

def runRound(grove: Grove, elves: List[Elf], dirOffset: Int): Int = {
    val proposed = mutable.Map[Pos, List[Elf]]()

    val directions = rotateLeft(Seq(North, South, West, East), (dirOffset - 1) % 4)

    elves.foreach { elf =>
        grove.adjacent(elf.pos) match {
            case Nil =>
            case elves =>
                val proposedMove = directions.find {
                    case North => !elves.exists(_.pos.y == elf.pos.y - 1)
                    case South => !elves.exists(_.pos.y == elf.pos.y + 1)
                    case West  => !elves.exists(_.pos.x == elf.pos.x - 1)
                    case East  => !elves.exists(_.pos.x == elf.pos.x + 1)
                }

                proposedMove.map(elf.pos.move).foreach { pos =>
                    proposed.updateWith(pos) {
                        case None     => Some(elf :: Nil)
                        case Some(xs) => Some(elf :: xs)
                    }
                }
        }
    }

    var moved = 0

    proposed.foreach {
        case (pos, elf :: Nil) =>
            grove.move(elf, pos)
            moved += 1
        case _ =>
    }

    moved
}

@main def day23(): Unit = {
    val sep = System.lineSeparator()

    val example = false
    val input = util.readInput(23, example)
    val rows = input.split(sep)

    val width = rows(0).length
    val height = rows.length

    val grove = Grove(width * 10, height * 10)

    var elves: List[Elf] = Nil

    input
        .split(sep)
        .map(_.toCharArray)
        .zipWithIndex
        .foreach((row, y) => {
            for
                (c, x) <- row.zipWithIndex if c == '#'
                pos = Pos(x, y)
                elf = Elf(pos)
            do
                grove.set(pos, elf)
                elves = elf :: elves
        })

    var round = 0
    var done = false
    var part1 = 0
    while (!done) {
        round += 1
        done = runRound(grove, elves, round) == 0

        if (round == 10) {
            part1 = grove.emptyTiles
        }
    }

    println(grove.draw)

    println(s"Part 1: $part1")
    println(s"Part 2: $round")

}
