package day21
import util.*
import java.io.File
import scala.math.Ordering

type Operator = "+" | "-" | "*" | "/"

case class BinaryOp(op: Operator, lhs: String, rhs: String)

type Expression = Long | BinaryOp

def inverse(expr: Expression, cache: Map[String, Node]): (Long => Long) = expr match {
    case BinaryOp(op, lhs, rhs) =>
        val l = cache(lhs)
        val r = cache(rhs)

        op match {
            case _: "+" =>
                val c = if l.fx then r else l

                (y: Long) => y - c.eval(cache) // fx(x) + c = y => f(x) = y - c

            case _: "-" =>
                val c = if l.fx then r else l

                if l.fx then (y: Long) => y + c.eval(cache) // f(x) - c = y => f(x) = y + c
                else (y: Long) => c.eval(cache) - y // c - f(x) = y => f(x) = c - y

            case _: "*" =>
                val c = if l.fx then r else l

                (y: Long) => y / c.eval(cache) // f(x) * c = y => f(x) = y / c

            case _: "/" =>
                if l.fx then (y: Long) => y * r.eval(cache) // f(x) / c = y => f(x) = y * c
                else (y: Long) => l.eval(cache) / y // c / f(x) = y => f(x) = c / y
        }
}

case class Node(id: String, expr: Expression):
    private var value: Option[Long] = None
    private var _fx: Boolean = false

    def fx: Boolean = _fx

    def eval(cache: Map[String, Node]): Long = {
        if (value.isEmpty) {
            value = expr match {
                case v: Long =>
                    _fx = id == "humn"
                    Some(v)
                case BinaryOp(op, lhs, rhs) =>
                    val leftNode = cache(lhs)
                    val rightNode = cache(rhs)
                    val l = leftNode.eval(cache)
                    val r = rightNode.eval(cache)
                    val result = op match {
                        case _: "+" => l + r
                        case _: "-" => l - r
                        case _: "*" => l * r
                        case _: "/" => l / r
                    }

                    _fx = leftNode.fx || rightNode.fx || id == "humn"
                    Some(result)
            }

        }

        value.get
    }

@main def day21() = {
    val sep = System.lineSeparator()

//    val input = util.readInput(21, example = true)
    val input = util.readInput(21, example = false)

    val opRegex = raw"(\S+): (\S+) (.) (\S+)".r
    val valueRegex = raw"(\S+): (\d+)".r

    val xs = input.split(sep).map {
        case opRegex(id, lhs, op: Operator, rhs) => Node(id, BinaryOp(op, lhs, rhs))
        case valueRegex(id, value)               => Node(id, value.toLong)
    }

    val cache = xs.map(x => (x.id, x)).toMap
    val root = cache("root")
    val (lhs, rhs) = root.expr match { case b: BinaryOp => (cache(b.lhs), cache(b.rhs)) }

    val rootValue = root.eval(cache)
    println(s"Part 1: $rootValue")

    var ops = List[Long => Long]()
    var fxNode = root
    while (fxNode.id != "humn") {
        if (fxNode.id != "root") {
            ops = inverse(fxNode.expr, cache) :: ops
        }

        fxNode = fxNode.expr match {
            case b: BinaryOp =>
                val lhs = cache(b.lhs)
                val rhs = cache(b.rhs)

                if lhs.fx then lhs else rhs
        }
    }

    val goal = (if lhs.fx then rhs else lhs).eval(cache)

    val result = ops.foldRight(goal)((f, y) => f(y))
    println(s"Part 2: $result")
}
