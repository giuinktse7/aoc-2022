package day10

import scala.collection.immutable

enum Instruction:
    case AddX(value: Int)
    case Noop

def infiniteRange(start: Int, step: Int): LazyList[Int] = start #:: infiniteRange(start + step, step)

class VM(var program: List[Instruction], var register: Int):
    private var work = 0
    private var currOp: Option[Instruction] = None

    def tick(): Unit = {
        if (currOp.isEmpty) {
            nextOp()
        }

        work = math.max(work - 1, 0)

        if (work == 0) {
            currOp.foreach {
                case Instruction.AddX(value) => register += value
                case _                       =>
            }

            currOp = None
        }
    }

    private def nextOp(): Unit = {
        if (program.isEmpty) {
            return
        }

        val op :: rest = program
        this.program = rest

        currOp = Some(op)
        work = op match {
            case Instruction.Noop    => 1
            case Instruction.AddX(_) => 2
        }
    }

    def isIdle: Boolean = program.isEmpty && currOp.isEmpty
