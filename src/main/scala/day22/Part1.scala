package day22
import util.*
import java.io.File
import scala.math.Ordering
import Tile.*
import Direction.*

enum Direction(val value: Int):
    case East extends Direction(0)
    case South extends Direction(1)
    case West extends Direction(2)
    case North extends Direction(3)

def stepDelta(direction: Direction) = {
    direction match {
        case North => (0, -1)
        case West  => (-1, 0)
        case East  => (1, 0)
        case South => (0, 1)
    }
}

def topLeft(face: Int, faceSideLength: Int): (Int, Int) =
    val s = faceSideLength
    val (xSteps, ySteps) = face match {
        case 1 => (2, 0)
        case 2 => (0, 1)
        case 3 => (1, 1)
        case 4 => (2, 1)
        case 5 => (2, 2)
        case 6 => (3, 2)
    }

    (xSteps * s, ySteps * s)

def topLeft2(face: Int, faceSideLength: Int): (Int, Int) =
    val s = faceSideLength
    val (xSteps, ySteps) = face match {
        case 1 => (1, 0)
        case 2 => (2, 0)
        case 3 => (1, 1)
        case 4 => (1, 2)
        case 5 => (0, 2)
        case 6 => (0, 3)
    }

    (xSteps * s, ySteps * s)

def rotateLeft(from: Direction) = from match {
    case West  => South
    case South => East
    case East  => North
    case North => West
}

def rotateRight(from: Direction) = from match {
    case South => West
    case West  => North
    case North => East
    case East  => South
}

def cubeFace(x: Int, y: Int, faceSideLength: Int): Int = {
    val s = faceSideLength
    val col = math.floor(x / s).toInt

    val row = math.floor(y / s).toInt
    (col, row) match {
        case (2, 0) => 1
        case (0, 1) => 2
        case (1, 1) => 3
        case (2, 1) => 4
        case (2, 2) => 5
        case (3, 2) => 6
    }
}

def cubeFace2(x: Int, y: Int, faceSideLength: Int): Int = {
    val s = faceSideLength
    val col = math.floor(x / s).toInt

    val row = math.floor(y / s).toInt
    (col, row) match {
        case (1, 0) => 1
        case (2, 0) => 2
        case (1, 1) => 3
        case (1, 2) => 4
        case (0, 2) => 5
        case (0, 3) => 6
    }
}

class BoardMap(width: Int, height: Int, cubeSideLength: Int) {
    val data = Array.fill(height, width)(Empty)

    def set(x: Int, y: Int, tile: Tile) = {
        data(y)(x) = tile
    }

    def at(x: Int, y: Int) = {
        if y < 0 || y >= height || x < 0 || x >= width then Empty
        else data(y)(x)
    }

    def wrap(x: Int, y: Int, direction: Direction): (Int, Int) = {
        direction match {
            case West =>
                val x = data(y).lastIndexWhere(_ != Empty)
                (x, y)
            case East =>
                val x = data(y).indexWhere(_ != Empty)
                (x, y)
            case North =>
                (y until height).findLast(y => at(x, y) != Empty).map(y => (x, y)).get
            case South =>
                (0 until y).find(y => at(x, y) != Empty).map(y => (x, y)).get
        }
    }

    def wrapCube(x0: Int, y0: Int, direction: Direction): (Int, Int, Direction) = {
        // height = s * 3
        // width = s * 4
        val s = cubeSideLength
        val face = cubeFace(x0, y0, s)

        val x = x0 % s
        val y = y0 % s
        val m = s - 1

        val (nextFace, (dx, dy), nextDir) = face match {
            case 1 =>
                direction match {
                    case South => (4, (x, 0), South)
                    case North => (2, (x, 0), South)
                    case West  => (3, (y, 0), South)
                    case East  => (6, (m, m - y), West)
                }
            case 2 =>
                direction match {
                    case South => (5, (m - x, m), North)
                    case North => (1, (m - x, m), South)
                    case West  => (6, (m - y, m), North)
                    case East  => (3, (0, y), East)
                }
            case 3 =>
                direction match {
                    case South => (5, (0, m - y), East)
                    case North => (1, (0, x), East)
                    case West  => (2, (m, y), West)
                    case East  => (4, (0, y), East)
                }
            case 4 =>
                direction match {
                    case South => (5, (x, 0), South)
                    case North => (1, (x, m), North)
                    case West  => (3, (m, y), West)
                    case East  => (6, (m - y, 0), South)
                }
            case 5 =>
                direction match {
                    case South => (2, (m - x, m), North)
                    case North => (4, (x, m), North)
                    case West  => (3, (m - y, m), North)
                    case East  => (6, (x, 0), East)
                }
            case 6 =>
                direction match {
                    case South => (2, (0, m - x), East) // ?
                    case North => (4, (m, m - x), West)
                    case West  => (5, (m, y), West)
                    case East  => (1, (m, m - y), West)
                }
        }

        val (x2, y2) = topLeft(nextFace, cubeSideLength)
        (x2 + dx, y2 + dy, nextDir)
    }

    def wrapCube2(x0: Int, y0: Int, direction: Direction): (Int, Int, Direction) = {
        val s = cubeSideLength
        val face = cubeFace2(x0, y0, s)

        val x = x0 % s
        val y = y0 % s
        val m = s - 1

        val (nextFace, (dx, dy), nextDir) = face match {
            case 1 =>
                direction match {
                    case South => (3, (x, 0), South)
                    case North => (6, (0, x), East)
                    case West  => (5, (0, m - y), East)
                    case East  => (2, (0, y), East)
                }
            case 2 =>
                direction match {
                    case South => (3, (m, x), West)
                    case North => (6, (x, m), North)
                    case West  => (1, (m, y), West)
                    case East  => (4, (m, m - y), West)
                }
            case 3 =>
                direction match {
                    case South => (4, (x, 0), South)
                    case North => (1, (x, m), North)
                    case West  => (5, (y, 0), South)
                    case East  => (2, (y, m), North) ////
                }
            case 4 =>
                direction match {
                    case South => (6, (m, x), West)
                    case North => (3, (x, m), North)
                    case West  => (5, (m, y), West) // ?
                    case East  => (2, (m, m - y), West) // ?
                }
            case 5 =>
                direction match {
                    case South => (6, (x, 0), South)
                    case North => (3, (0, x), East)
                    case West  => (1, (0, m - y), East)
                    case East  => (4, (0, y), East)
                }
            case 6 =>
                direction match {
                    case South => (2, (x, 0), South)
                    case North => (5, (x, m), North)
                    case West  => (1, (y, 0), South)
                    case East  => (4, (y, m), North)
                }
        }

        val (x2, y2) = topLeft2(nextFace, cubeSideLength)
        (x2 + dx, y2 + dy, nextDir)
    }

    def draw: String = {
        val data = for
            y <- 0 until height
            x <- 0 until width
        yield at(x, y) match {
            case Empty  => ' '
            case Ground => '.'
            case Left   => '<'
            case Right  => '>'
            case Up     => '^'
            case Down   => 'v'
            case Wall   => '#'
        }

        data.grouped(width).map(_.mkString).mkString("\n")
    }

    def walk(s: State, steps: Int): Unit = {
        var (x, y) = (s.x, s.y)

        val dt = s.direction match {
            case North => Up
            case East  => Right
            case West  => Left
            case South => Down
        }
        set(x, y, dt)

        var (dx, dy) = stepDelta(s.direction)
        var i = 0
        while (i < steps) {
            val currDir = s.direction
            val dirTile = s.direction match {
                case North => Up
                case East  => Right
                case West  => Left
                case South => Down
            }
            set(x, y, dirTile)
            val prevX = x
            val prevY = y
            x += dx
            y += dy
            val tile = at(x, y) match {
                case Empty =>
//                    val (wx, wy) = wrap(x, y, s.direction)
                    val (wx, wy, nextDir) = wrapCube2(prevX, prevY, s.direction)
                    x = wx
                    y = wy

                    // Update direction
                    s.direction = nextDir
                    val (dxNew, dyNew) = stepDelta(s.direction)
                    dx = dxNew
                    dy = dyNew

                    at(x, y)
                case t => t
            }

            if (tile == Wall) {
                s.direction = currDir
                return
            }

            s.x = x
            s.y = y

            i += 1
        }

        val dirTile = s.direction match {
            case North => Up
            case East  => Right
            case West  => Left
            case South => Down
        }
        set(x, y, dirTile)
    }
}

case class State(var x: Int, var y: Int, var direction: Direction)

def tokenize(xs: Array[Char]): List[String] = {
    var tokens: List[String] = Nil
    var start = 0
    var cursor = 0
    while (cursor < xs.length) {
        xs(cursor) match {
            case c @ ('L' | 'R') =>
                if (start != cursor) {
                    tokens = c.toString :: xs.slice(start, cursor).mkString :: tokens
                } else {
                    tokens = c.toString :: tokens
                }
                start = cursor + 1
            case _ =>
        }

        cursor += 1
    }

    tokens = xs.slice(start, cursor).mkString :: tokens

    tokens.reverse
}

@main def day22_part1() = {
    val sep = System.lineSeparator()

    val example = false

//    val input = util.readInput(22, example = false)
    val input = util.readInput(22, example)
    val cubeSideLength = if example then 4 else 50

    val Array(mapData, instructions) = input.split(s"$sep$sep")
    val rows = mapData.split(sep)

    val width = rows.map(_.length).max
    val height = rows.length

    println(s"$width, $height")

    val map = BoardMap(width, height, cubeSideLength)

    var start: Option[(Int, Int)] = None

    // Construct map
    mapData
        .split(sep)
        .map(_.toCharArray)
        .zipWithIndex
        .foreach((row, y) => {
            for
                (c, x) <- row.zipWithIndex
                tile = c match {
                    case ' ' => Empty
                    case '.' => Ground
                    case '#' => Wall
                }
            do {
                if (start.isEmpty && tile == Ground) {
                    start = Some((x, y))
                }

                map.set(x, y, tile)
            }
        })

    //    println(mapData)

    println("\n------------------------------\n")

    val (startX, startY) = start.get

    println(s"$startX, $startY")

    val s = State(startX, startY, East)
    val moves: List[String | Int] = tokenize(instructions.toCharArray).map {
        case k if k.forall(_.isDigit) => k.toInt
        case c                        => c
    }

    moves.foreach {
        case "L" =>
            s.direction = rotateLeft(s.direction)
            print("L")
        case "R" =>
            s.direction = rotateRight(s.direction)
            print("R")
        case steps: Int =>
            print(steps)
            map.walk(s, steps)
        //            println(
        //              s"\nWalked $steps steps (Now facing ${s.direction.value}). Map:\n----------------------------------"
        //            )
        //            println(map.draw)
    }
    println()

    val row = s.y + 1
    val column = s.x + 1
    val facing = s.direction.value

    println(s"row=$row, column=$column, facing=$facing")

    println()
    println("Final map:\n----------------------------------")
    println(map.draw)
    println()

    val password = row * 1000 + column * 4 + facing
    println(password)
}
