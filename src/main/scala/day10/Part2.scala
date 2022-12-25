package day10

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class CRT(width: Int, height: Int):
    private val grid = Array.ofDim[Boolean](width * height)

    def draw(pos: Int, spritePos: Int, spriteWidth: Int): Unit = {
        val dx = (spriteWidth - 1) / 2
        val x = pos % width
        grid(pos) = (spritePos - dx to spritePos + dx).contains(x)
    }

    def display: String = {
        grid.map(x => if x then "#" else ".")
            .grouped(width)
            .map(_.mkString)
            .mkString("\n")
    }

@main def day10_part2() = {
    val sep = System.lineSeparator()
    val instructionRegex = raw"(\w+)(?: (.*))?".r

    val instructions = util
        .readInput(10)
        .split(sep)
        .map {
            case "noop"                          => Instruction.Noop
            case instructionRegex("addx", value) => Instruction.AddX(value.toInt)
        }
        .toList

    val vm = VM(program = instructions, register = 1)
    val crt = CRT(40, 6)

    breakable {
        for (cycle <- infiniteRange(1, 1)) {
            crt.draw(cycle - 1, vm.register, 3)
            vm.tick()

            if (vm.isIdle) {
                break
            }
        }
    }

    println(crt.display)
}
