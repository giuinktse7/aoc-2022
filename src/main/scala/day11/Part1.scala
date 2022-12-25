package day11
import org.graalvm.compiler.nodes.NamedLocationIdentity.mutable
import util.*

import java.io.File
import scala.math.Ordering

@main def day11_part1() = {
    val input = util.readInput(11)
    val monkeys = input.split(s"$sep$sep").map(parseMonkey).sortBy(_.id).toArray

    (1 to 20).foreach(round => {
        monkeys.foreach(monkey => {
            monkey.seen += monkey.items.length

            monkey.items.foreach(item => {
                val worry = monkey.worryFn(item) / 3
                val monkeyId: Int = monkey.nextMonkey(worry)
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
