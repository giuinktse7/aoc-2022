package day8

import scala.collection.immutable

def infiniteRange(start: Int, dx: Int): LazyList[Int] = start #:: infiniteRange(start + dx, dx)

object Direction {
    def unapply(direction: Direction) =
        Some((direction.dx, direction.dy))
}

enum Direction(x: Int, y: Int):
    def dx = x
    def dy = y
    case Up extends Direction(0, -1)
    case Left extends Direction(-1, 0)
    case Down extends Direction(0, 1)
    case Right extends Direction(1, 0)

class Forest(data: Array[Array[Int]]) {
    def width: Int = data(0).length
    def height: Int = data.length

    def locations: Seq[(Int, Int)] = {
        for
            y <- 0 until height
            x <- 0 until width
        yield (x, y)
    }

    def tree(x: Int, y: Int): Int = data(y)(x)
    def contains(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
    def isEdge(x: Int, y: Int): Boolean = x == 0 || x == width - 1 || y == 0 || y == height - 1
}
