package day5
import day5.common.solveDay5
import util.*

import java.io.File
import scala.collection.mutable
import scala.math.Ordering
import scala.collection.mutable.Stack

@main def day5_part2() = {
    val input = util.readInput(5)

    val result = solveDay5(
      input,
      { case (data, (quantity, from, to)) =>
          val (stack, rest) = data(from).splitAt(quantity)
          data.updated(from, rest).updatedWith(to)(_.map(stack ::: _))
      }
    )

    println(result)
}
