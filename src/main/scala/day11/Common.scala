package day11

val sep = System.lineSeparator()

case class Monkey(
    id: Int,
    var items: Vector[BigInt],
    worryFn: BigInt => BigInt,
    nextMonkey: BigInt => Int,
    divisor: Int,
    var seen: Int
)

def parseOp(op: String): (BigInt, BigInt) => BigInt = op match {
    case "+" => _ + _
    case "*" => _ * _
}

def parseOperation(x1: String, op: String, x2: String): BigInt => BigInt = {
    (x1, parseOp(op), x2) match {
        case ("old", op, "old") => old => op(old, old)
        case (x1, op, "old")    => old => op(x1.toInt, old)
        case ("old", op, x2)    => old => op(old, x2.toInt)
        case (x1, op, x2)       => _ => op(x1.toInt, x2.toInt)
    }
}

def parseMonkey(data: String): Monkey = {
    val attribute = raw"([^:]*): (.*)".r

    val operation = raw"new = ([^ ]+) ([^ ]+) ([^ ]+)".r

    var divisor = 0
    var monkeyIfTrue = 0
    var monkeyIfFalse = 0

    def lastInt(s: String) = { s.splitAt(s.lastIndexOf(' '))._2.trim.toInt }

    val monkeyLine :: lines = data.split(sep).toList
    val monkeyId = lastInt(monkeyLine.replace(":", ""))

    var monkey = Monkey(monkeyId, Vector(), x => x, x => 0, seen = 0, divisor = 0)

    lines
        .map { case attribute(k, v) => (k.trim, v.trim) }
        .foreach {
            case ("Starting items", xs) =>
                monkey = monkey.copy(items = xs.split(",").map(x => BigInt.long2bigInt(x.trim.toLong)).toVector)
            case ("Operation", args) =>
                args match {
                    case operation(x1, op, x2) =>
                        monkey = monkey.copy(worryFn = parseOperation(x1, op, x2))
                }
            case ("Test", args)     => divisor = lastInt(args)
            case ("If true", args)  => monkeyIfTrue = lastInt(args)
            case ("If false", args) => monkeyIfFalse = lastInt(args)
            case line               => println(line)
        }

    val nextMonkeyFn = (item: BigInt) => if item % divisor == 0 then monkeyIfTrue else monkeyIfFalse
    monkey.copy(nextMonkey = nextMonkeyFn, divisor = divisor)
}

case class State(monkeys: Array[Monkey])
