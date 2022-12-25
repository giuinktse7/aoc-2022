package day15
import util.*

import java.io.File
import scala.collection.immutable.::
import scala.math.Ordering

case class Pos(x: Int, y: Int):
    def +(delta: (Int, Int)): Pos = Pos(x + delta._1, y + delta._2)
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)

def manhattanDistance(a: Pos, b: Pos) = (a.x - b.x).abs + (a.y - b.y).abs

case class Sensor(pos: Pos, closestBeaconPos: Pos):
    var buddies = List[Sensor]()
    val range = manhattanDistance(pos, closestBeaconPos)
    def intersectsY(y: Int) = (pos.y - y).abs <= range

    def adjacent(that: Sensor) = manhattanDistance(this.pos, that.pos) <= this.range + that.range + 1

    def contains(pos: Pos) = manhattanDistance(this.pos, pos) <= range

    def edge =
        val d = range + 1
        for
            dx <- -d to d
            y0 = d - dx.abs
            dy <- y0 :: (if y0 == 0 then Nil else -y0 :: Nil)
        yield pos + Pos(dx, dy)

def parseSensors(input: String) = input
    .split(System.lineSeparator())
    .map(x => raw"x=(-?\d+), y=(-?\d+)".r.findAllMatchIn(x).map(_.subgroups.map(_.toInt)).toList)
    .map { case (x :: y :: _) :: (bx :: by :: _) :: _ => Sensor(Pos(x, y), Pos(bx, by)) }
