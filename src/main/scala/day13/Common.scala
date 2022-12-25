package day13
import day13.Status.{Failure, Success, Undecided}

import scala.annotation.tailrec

case class Data(values: Vector[Int | Data] = Vector()):
    def :+(value: Int | Data): Data = Data(values :+ value)
    def size: Int = values.size
    def apply(i: Int): Int | Data = values(i)

class Tokenizer(data: String):
    private var cursor = 0

    def hasNext: Boolean = cursor < data.length

    @tailrec
    private def findToken(): (Int, String) = {
        data(cursor) match {
            case ' ' | ',' =>
                cursor += 1
                findToken()
            case a @ ('[' | ']') =>
                (1, a.toString)
            case _ =>
                val number = data.view.drop(cursor).takeWhile(_.isDigit).mkString
                (number.length, number)
        }
    }

    def nextToken(): String = {
        val (chars, token) = findToken()

        cursor += chars
        token
    }

def parse(tokenizer: Tokenizer): Data = {
    var xs: Data = Data()
    while (tokenizer.hasNext) {
        tokenizer.nextToken() match {
            case "["    => xs = xs :+ parse(tokenizer)
            case "]"    => return xs
            case number => xs = xs :+ number.toInt
        }
    }

    xs
}

enum Status:
    case Undecided, Success, Failure

def compare(a: Data, b: Data): Status = {
    var i = 0
    while (i < a.size) {
        if (i >= b.size) {
            return Failure
        }

        val status = (a(i), b(i)) match {
            case (x: Int, y: Int) if x == y => Undecided
            case (x: Int, y: Int) if x > y  => Failure
            case (x: Int, y: Int) if x < y  => Success
            case (x: Int, y: Data)          => compare(Data(Vector(x)), y)
            case (x: Data, y: Int)          => compare(x, Data(Vector(y)))
            case (x: Data, y: Data)         => compare(x, y)
        }

        if (status != Undecided) {
            return status
        }

        i += 1
    }

    if (i < b.size) {
        return Success
    }

    Undecided
}
