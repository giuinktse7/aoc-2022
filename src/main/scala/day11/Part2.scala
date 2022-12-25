package day11
import org.graalvm.compiler.nodes.NamedLocationIdentity.mutable
import util.*

import java.io.File
import scala.math.Ordering

@main def day11_part2(): Unit = {
    val input = util.readInput(11)
    val monkeys = input.split(s"$sep$sep").map(parseMonkey).sortBy(_.id).toArray

    val M = monkeys.map(m => m.divisor).product

    (1 to 10000).foreach(_ => {
        monkeys.foreach(monkey => {
            monkey.seen += monkey.items.length

            monkey.items.foreach(item => {
                val worry = monkey.worryFn(item % M)
                val monkeyId = monkey.nextMonkey(worry)
                monkeys(monkeyId).items = monkeys(monkeyId).items :+ worry
            })

            monkey.items = Vector()
        })
    })

    val result =
        monkeys
            .map(m => (m.id, m.seen))
            .toList
            .map((_, count) => count.toLong)
            .sorted
            .takeRight(2)
            .product

    println(result)
}
