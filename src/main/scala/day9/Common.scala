package day9

case class Vec(x: Int, y: Int):
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)
    def -(that: Vec): Vec = Vec(x - that.x, y - that.y)
    def abs: Vec = Vec(x.abs, y.abs)
    def max: Int = math.max(x, y)
    def sign: Vec = Vec(math.signum(x), math.signum(y))

case class State(head: Vec, knots: List[Vec], seen: Set[Vec])

def run(input: String, knotCount: Int) = {
    val sep = System.lineSeparator()
    val step = raw"([R|U|L|D]) (\d+)".r

    val zero = Vec(0, 0)
    val s0 = State(zero, List.fill(knotCount - 1)(zero), Set(zero))

    input
        .split(sep)
        .toList
        .flatMap(step.unapplySeq)
        .flatMap { case dir :: x :: _ => Some(dir, x.toInt); case _ => None }
        .flatMap((direction, delta) =>
            List.fill(delta)(direction match {
                case "R" => Vec(1, 0)
                case "L" => Vec(-1, 0)
                case "U" => Vec(0, -1)
                case "D" => Vec(0, 1)
            })
        )
        .foldLeft(s0)((state, step) => {
            val head = state.head + step
            val knots = (head :: state.knots).toArray

            for (i <- 1 until knots.length) {
                val (k1, k2) = (knots(i - 1), knots(i))
                val diff = k1 - k2
                knots(i) = if diff.abs.max > 1 then k2 + diff.sign else k2
            }

            State(head, knots.tail.toList, state.seen + knots.last)
        })
        .seen
        .size
}
