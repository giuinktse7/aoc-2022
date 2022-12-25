package day2
import util.*

import java.io.File
import scala.::
import scala.collection.immutable.HashMap
import scala.math.Ordering

@main def day2_part2() = {
  val sep = System.lineSeparator()

  val input = util.readInput(2)
  
  val choiceValue = HashMap("A" -> 1, "B" -> 2, "C" -> 3)
  
  val result = input.split(sep)
    .map(x => x.split(" ").toList match {
      case a :: b :: _ =>
        b match {
          // Lose
          case "X" => choiceValue(a match {
            case "A" => "C"
            case "B" => "A"
            case "C" => "B"
          })

          // Draw
          case "Y" => choiceValue(a) + 3

          // Win
          case "Z" => choiceValue(a match {
            case "A" => "B"
            case "B" => "C"
            case "C" => "A"
          }) + 6
        }
      case _ => 0
    })
    .sum

  println(result)
}
