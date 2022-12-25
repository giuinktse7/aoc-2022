package day7
import day7.common.{allSubPaths, createFileSystem, parent}
import util.*

import java.io.File
import java.nio.file.{Path, Paths}
import scala.math.Ordering
import scala.collection.immutable
import scala.util.chaining.scalaUtilChainingOps

@main def day7_part1() = {
    val input = util.readInput(7)

    val filesystem = createFileSystem(input)

    val dirs = filesystem.keys
        .flatMap(_.pipe(parent).pipe(allSubPaths))
        .toList
        .distinct

    val result = dirs
        .map(dir => filesystem.filter((file, _) => file.startsWith(dir)).map((_, size) => size).sum)
        .filter(_ <= 100000)
        .sum

    println(s"Result: $result")
}
