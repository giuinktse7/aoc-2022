package day8

import scala.language.postfixOps

@main def day8_part1() = {
    val sep = System.lineSeparator()

    val input = util.readInput(8)

    val data = input.split(sep).map(_.toCharArray.map(_.toString.toInt))
    val forest = Forest(data)

    val result = forest.locations.count((x0, y0) =>
        Direction.values.exists { case Direction(dx, dy) =>
            val self = forest.tree(x0, y0)
            val xs = infiniteRange(x0 + dx, dx)
            val ys = infiniteRange(y0 + dy, dy)

            val obstruction = xs
                .zip(ys)
                .takeWhile((x, y) => forest.contains(x, y))
                .exists((x, y) => forest.tree(x, y) >= self)

            !obstruction
        }
    )

    println(result)
}
