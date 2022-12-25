package day24
import util.recurse
import scala.collection.mutable

def AStar(
    start: Pos,
    goal: Pos,
    h: Node => Float,
    neighbors: (Node, Int) => List[Node],
    drawMap: (Pos, Int) => Unit,
    slow: Boolean = false
): (List[Pos], Int) = {
    val cameFrom = mutable.Map[Node, Node]()

    val zero = Node(start, 0)

    val gScore = mutable.Map[Node, Int](zero -> 0).withDefaultValue(Int.MaxValue)
    val fScore = mutable.Map[Node, Float](zero -> h(zero)).withDefaultValue(Float.MaxValue)

    val set = mutable.PriorityQueue.empty[Node](Ordering.by(fScore).reverse)
    set.addOne(zero)
    val set2 = mutable.Set[Node](zero)

    while (set.nonEmpty) {
        val current = set.dequeue()
        set2.remove(current)

        if (current.pos == goal) {
            return (recurse(current, cameFrom.get).map(_.pos), current.t)
        } else {
            val t = gScore(current)

            val ns = neighbors(current, t)
            ns.foreach { neighbor =>
                val score = gScore(current) + 1
                if (score < gScore(neighbor)) {
                    cameFrom(neighbor) = current

                    gScore(neighbor) = score
                    fScore(neighbor) = score + h(neighbor)

                    if (!set2.contains(neighbor)) {
                        set.addOne(neighbor)
                        set2.addOne(neighbor)
                    }
                }
            }
        }
    }

    (Nil, 0)
}
