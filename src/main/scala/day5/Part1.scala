package day5
import day5.common.solveDay5
import util.*

import java.io.File
import scala.collection.mutable
import scala.math.Ordering
import scala.collection.mutable.Stack

@main def day5_part1() = {
    val input = util.readInput(5)

    val result = solveDay5(
      input,
      { case (crateStacks, (quantity, from, to)) =>
          (1 to quantity).foldLeft(crateStacks)((cs, _) => {
              val crate :: rest = cs(from)
              cs.updated(from, rest).updatedWith(to)(_.map(crate :: _))
          })
      }
    )

    println(result)
}
