package day8

import scala.language.postfixOps

@main def day8_part2(): Unit = {
    val sep = System.lineSeparator()

    val input = util.readInput(8)

    val data = input.split(sep).map(_.toCharArray.map(_.toString.toInt))
    val forest = Forest(data)

    val result =
        forest.locations
            .map((x0, y0) =>
                Direction.values.map { case Direction(dx, dy) =>
                    val self = forest.tree(x0, y0)
                    val xs = infiniteRange(x0 + dx, dx)
                    val ys = infiniteRange(y0 + dy, dy)

                    val path = xs.zip(ys).takeWhile((x, y) => forest.contains(x, y) && forest.tree(x, y) < self)

                    path.lastOption match {
                        case Some(x1, y1) => path.size + (if forest.isEdge(x1, y1) then 0 else 1)
                        case None         => 1
                    }
                }.product
            )
            .max

    println(result)
}
