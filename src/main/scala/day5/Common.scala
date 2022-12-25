package day5.common

def solveDay5(
    input: String,
    crateStrategy: (Map[Int, List[Char]], (Int, Int, Int)) => Map[Int, List[Char]]
) = {
    val sep = System.lineSeparator()
    val rawStackData :: steps :: Nil = input.split(s"$sep$sep").toList

    val validStep = raw"move (\d+) from (\d+) to (\d+)".r

    val stackData = rawStackData
        .split(sep)
        .map(_.toCharArray)
        .transpose
        .map(_.filter(_.isLetterOrDigit).toList)
        .foldLeft(Map[Int, List[Char]]()) {
            case (stacks, crates :+ stackId) => stacks + (stackId.toString.toInt -> crates)
            case (stacks, _)                 => stacks
        }

    steps
        .split(sep)
        .flatMap {
            case validStep(quantity, from, to) => Some(quantity.toInt, from.toInt, to.toInt)
            case _                             => None
        }
        .foldLeft(stackData)(crateStrategy)
        .toSeq
        .sortBy((k, _) => k)
        .flatMap((_, v) => v.headOption)
        .mkString
}
