package day10

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

@main def day10_part1() = {
    val sep = System.lineSeparator()
    val instructionRegex = raw"(\w+)(?: (.*))?".r

    val instructions = util
        .readInput(10, example = true)
        .split(sep)
        .map {
            case "noop"                          => Instruction.Noop
            case instructionRegex("addx", value) => Instruction.AddX(value.toInt)
        }
        .toList

    val vm = VM(program = instructions, register = 1)

    val notables = Set(20, 60, 100, 140, 180, 220).map(x => x.toLong)
    val pts = Array[(Int, Int)]()

    breakable {
        for (cycle <- infiniteRange(1, 1)) {
            if (notables.contains(cycle)) {
                pts :+ (cycle, vm.register)
            }

            vm.tick()

            if (vm.isIdle) {
                break
            }
        }
    }

    println(pts.map(_ * _).sum)
}
