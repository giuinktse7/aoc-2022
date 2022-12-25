//package day22
//import util.*
//import java.io.File
//import scala.math.Ordering
//import Tile.*
//import Direction.*
//
//enum Tile:
//    case Empty, Ground, Wall
//
//enum Direction(val value: Int):
//    case East extends Direction(0)
//    case South extends Direction(1)
//    case West extends Direction(2)
//    case North extends Direction(3)
//
//def stepDelta(direction: Direction) = {
//    direction match {
//        case North => (0, -1)
//        case West  => (-1, 0)
//        case East  => (1, 0)
//        case South => (0, 1)
//    }
//}
//
//def rotateLeft(from: Direction) = from match {
//    case West  => South
//    case South => East
//    case East  => North
//    case North => West
//}
//
//def rotateRight(from: Direction) = from match {
//    case South => West
//    case West  => North
//    case North => East
//    case East  => South
//}
//
//class BoardMap(width: Int, height: Int) {
//    val data = Array.fill(height, width)(Empty)
//
//    def set(x: Int, y: Int, tile: Tile) = {
//        data(y)(x) = tile
//    }
//
//    def at(x: Int, y: Int) = {
//        if y < 0 || y >= height || x < 0 || x >= width then Empty
//        else data(y)(x)
//    }
//
//    def wrap(x: Int, y: Int, direction: Direction): (Int, Int) = {
//        direction match {
//            case West =>
//                val x = data(y).lastIndexWhere(_ != Empty)
//                (x, y)
//            case East =>
//                val x = data(y).indexWhere(_ != Empty)
//                (x, y)
//            case North =>
//                (y until height).findLast(y => at(x, y) != Empty).map(y => (x, y)).get
//            case South =>
//                (0 until y).find(y => at(x, y) != Empty).map(y => (x, y)).get
//        }
//    }
//
//    def walk(s: State, steps: Int): Unit = {
//        var (x, y) = (s.x, s.y)
//
//        val (dx, dy) = stepDelta(s.direction)
//        var i = 0
//        while (i < steps) {
//            x += dx
//            y += dy
//            val tile = at(x, y) match {
//                case Empty =>
//                    val (wx, wy) = wrap(x, y, s.direction)
//                    x = wx
//                    y = wy
//                    at(x, y)
//                case t => t
//            }
//
//            if (tile == Wall) {
//                return
//            }
//
//            s.x = x
//            s.y = y
//
//            i += 1
//        }
//    }
//}
//
//case class State(var x: Int, var y: Int, var direction: Direction)
//
//def tokenize(xs: Array[Char]): List[String] = {
//    var tokens: List[String] = Nil
//    var start = 0
//    var cursor = 0
//    while (cursor < xs.length) {
//        xs(cursor) match {
//            case c @ ('L' | 'R') =>
//                if (start != cursor) {
//                    tokens = c.toString :: xs.slice(start, cursor).mkString :: tokens
//                } else {
//                    tokens = c.toString :: tokens
//                }
//                start = cursor + 1
//            case _ =>
//        }
//
//        cursor += 1
//    }
//
//    tokens = xs.slice(start, cursor).mkString :: tokens
//
//    tokens.reverse
//}
//
//@main def day22_part1() = {
//    val sep = System.lineSeparator()
//
//    val input = util.readInput(22, example = false)
//    //    val input = util.readInput(22, example = true)
//
//    val Array(mapData, instructions) = input.split(s"$sep$sep")
//    val rows = mapData.split(sep)
//
//    val width = rows.map(_.length).max
//    val height = rows.length
//
//    println(s"$width, $height")
//
//    val map = BoardMap(width, height)
//
//    var start: Option[(Int, Int)] = None
//
//    // Construct map
//    mapData
//        .split(sep)
//        .map(_.toCharArray)
//        .zipWithIndex
//        .foreach((row, y) => {
//            for
//                (c, x) <- row.zipWithIndex
//                tile = c match {
//                    case ' ' => Empty
//                    case '.' => Ground
//                    case '#' => Wall
//                }
//            do {
//                if (start.isEmpty && tile == Ground) {
//                    start = Some((x, y))
//                }
//
//                map.set(x, y, tile)
//            }
//        })
//
//    //    println(mapData)
//
//    println("\n------------------------------\n")
//
//    val (startX, startY) = start.get
//
//    val s = State(startX, startY, East)
//    val moves: List[String | Int] = tokenize(instructions.toCharArray).map {
//        case k if k.forall(_.isDigit) => k.toInt
//        case c                        => c
//    }
//
//    moves.foreach {
//        case "L"        => s.direction = rotateLeft(s.direction)
//        case "R"        => s.direction = rotateRight(s.direction)
//        case steps: Int => map.walk(s, steps)
//    }
//
//    val row = s.y + 1
//    val column = s.x + 1
//    val facing = s.direction.value
//
//    val password = row * 1000 + column * 4 + facing
//    println(password)
//}
