package day2
import util.*

import java.io.File
import scala.::
import scala.collection.immutable.HashMap
import scala.math.Ordering

@main def day2_part1() = {
  val sep = System.lineSeparator()

  val input = util.readInput(2)
  
  val choiceValue = HashMap("A" -> 1, "B" -> 2, "C" -> 3)
  
  val result = input.split(sep)
    .map(x => x.split(" ").toList match {
      case a :: b :: _ => (a, b match {
        case "X" => "A"
        case "Y" => "B"
        case "Z" => "C"
        case _ => ""
      })
      case _ => ("", "")
    })
    .map {
      // Draw
      case (a, b) if a == b => choiceValue(a) + 3

      // Wins
      case ("A", "B") => choiceValue("B") + 6
      case ("B", "C") => choiceValue("C") + 6
      case ("C", "A") => choiceValue("A") + 6

      // Losses
      case ("A", "C") => choiceValue("C")
      case ("B", "A") => choiceValue("A")
      case ("C", "B") => choiceValue("B")

      case _ => 0
    }
    .sum

  println(result)
}
