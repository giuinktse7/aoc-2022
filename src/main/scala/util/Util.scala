package util

import scala.io.Source
import scala.util.Using

def readInputLines(day: Int, example: Boolean = false): Iterator[String] = {
    val path = if (example) s"data/$day/example.txt" else s"data/$day/input.txt"
    val lines = Using(Source.fromFile(path)) { source =>
        for (line <- source.getLines)
            yield line
    }

    lines.getOrElse(Iterator.empty)
}

def readInput(day: Int, example: Boolean = false): String = {
    val path = if (example) s"data/$day/example.txt" else s"data/$day/input.txt"
    val data = Using(Source.fromFile(path)) { _.mkString }
    data.getOrElse("")
}

def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / math.pow(10, 6) + "ms")
    result
}

def infinite[A](x: A): LazyList[A] = x #:: infinite(x)
def infiniteList[A](x: Seq[A]): LazyList[A] =
    x match {
        case y: LazyList[A] => y #::: infiniteList(y)
        case _ =>
            val k = x.foldRight(LazyList[A]())((a, b) => a #:: b)
            k #::: infiniteList(k)
    }

def signedPermutations(seq: Seq[Int]) = (seq.permutations ++ seq.map(x => -x).permutations).toList

def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
}

def rotateRight[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(size - (i % size)) ++ seq.take(size - (i % size))
}

def recurse[A](element: A, f: A => Option[A]): List[A] = {
    var result: List[A] = Nil

    var n: Option[A] = Some(element)
    while (n.nonEmpty) {
        result = n.get :: result
        n = n.flatMap(f)
    }

    result
}
