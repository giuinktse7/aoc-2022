package day20
import util.*
import java.io.File
import scala.math.Ordering

def mix(input: Seq[Long], nextNode: Array[Int], prevNode: Array[Int]) = {
    def follow(startNode: Int, n: Int) = {
        if (n < 0) {
            (0 to n.abs).foldLeft(startNode)((n, _) => prevNode(n))
        } else {
            (1 to n).foldLeft(startNode)((n, _) => nextNode(n))
        }
    }

    input.zipWithIndex.foreach { (rawValue, selfNode) =>
        rawValue % (input.length - 1) match {
            case 0 =>
            case value =>
                val targetNode = follow(selfNode, value.toInt)

                val targetNext = nextNode(targetNode)

                val selfPrev = prevNode(selfNode)
                val selfNext = nextNode(selfNode)

                if (targetNode == selfNode || targetNode == selfPrev) {} else {
                    if (selfNext == targetNode) {
                        // Fix self
                        nextNode(selfNode) = targetNext
                        prevNode(selfNode) = targetNode

                        // Fix target
                        nextNode(targetNode) = selfNode
                        prevNode(targetNode) = selfPrev

                        // Fix target next
                        prevNode(targetNext) = selfNode

                        // Fix self prev
                        nextNode(selfPrev) = targetNode
                    } else {
                        // Fix self
                        prevNode(selfNode) = targetNode
                        nextNode(selfNode) = targetNext

                        // Fix target
                        nextNode(targetNode) = selfNode

                        // Fix target next
                        prevNode(targetNext) = selfNode

                        // Fix self prev
                        nextNode(selfPrev) = selfNext

                        // Fix self next
                        prevNode(selfNext) = selfPrev
                    }
                }
        }
    }
}

def run(input: Seq[Long], decryptionKey: Long = 1, mixRounds: Int = 1) = {
    val xs = input.map(_ * decryptionKey)

    // (Index of original value) -> (Index of original value)
    val nextNode = (xs.indices.drop(1).toList :+ 0).toArray
    val prevNode = ((xs.length - 1) :: xs.indices.dropRight(1).toList).toArray

    (1 to mixRounds).foreach { _ => mix(xs, nextNode, prevNode) }

    def materializeList() = {
        var result = List[Long]()
        var node = 0
        while (result.size < xs.length) {
            result = result :+ xs(node)
            node = nextNode(node)
        }

        result
    }

    val result = materializeList()
    val i0 = result.indexOf(0)

    (1 to 3).map(x => (i0 + x * 1000) % result.size).map(result).sum
}

@main def day20(): Unit = {
    val sep = System.lineSeparator()

    val input = util
        .readInput(20)
        .split(sep)
        .map(_.toLong)

    // Part 1
    println("Part 1:")
    val result1 = time { run(input) }
    println(s"Part 1 result: $result1")

    // Part 2
    println("\nPart 2:")
    val result2 = time { run(input, decryptionKey = 811589153, mixRounds = 10) }
    println(s"Part 2: $result2")
}
