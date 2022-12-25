package day25

import util.recurse

import java.lang.Thread
import scala.collection.mutable

case class Node(index: Int, value: Int)

def AStar(
    start: Node,
    goal: Long,
    h: (Node, Long) => Long,
    neighbors: Node => List[Node]
): List[Node] = {
    val cameFrom = mutable.Map[Node, Node]()

    val zero = start

    val gScore = mutable.Map[Node, Long](zero -> 0L).withDefaultValue(Long.MaxValue)
    val fScore = mutable.Map[Node, Long](zero -> h(zero, 0L)).withDefaultValue(Long.MaxValue)

    val set = mutable.PriorityQueue.empty[Node](Ordering.by(fScore).reverse)
    set.addOne(zero)
    val set2 = mutable.Set[Node](zero)

    // https://math.stackexchange.com/a/1897065
    def expSum(n: Int) = 2 * (1 - math.pow(5, n)) / (1 - 5)

    while (set.nonEmpty) {
        val current = set.dequeue()

        set2.remove(current)

        if (gScore(current) == goal) {
            return recurse(current, cameFrom.get)
        } else if ((gScore(current) - goal).abs > expSum(current.index)) {
            // Do nothing
        } else {
            val ns = neighbors(current)
            ns.foreach { neighbor =>
                val score = gScore(current) + math.pow(5, neighbor.index).toLong * neighbor.value
                if ((score - goal).abs < (gScore(neighbor) - goal).abs) {
                    cameFrom(neighbor) = current

                    gScore(neighbor) = score
                    fScore(neighbor) = score + h(neighbor, gScore(neighbor))

                    if (!set2.contains(neighbor)) {
                        set.addOne(neighbor)
                        set2.addOne(neighbor)
                    }
                }
            }
        }
    }

    Nil
}
