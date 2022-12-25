package day1

@main def day1_part2() = {
  val sep = System.lineSeparator()

  val input = util.readInput(1)

  val result =
    input
      .split(s"$sep$sep")
      .map(group => group.split(sep).foldLeft(0)((a, b) => a + b.toInt))
      .sorted(Ordering.Int.reverse)
      .take(3)
      .sum

  println(result)
}