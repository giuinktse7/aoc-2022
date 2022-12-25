package day7
import day7.common.{allSubPaths, createFileSystem, parent}
import util.*

import java.io.File
import java.nio.file.{Path, Paths}
import scala.math.Ordering
import scala.collection.immutable
import scala.util.chaining.scalaUtilChainingOps

@main def day7_part2() = {
    val input = util.readInput(7)

    val filesystem = createFileSystem(input)

    val dirs: Map[String, Long] = filesystem.keys
        .flatMap(_.pipe(parent).pipe(allSubPaths))
        .toList
        .distinct
        .map(dir => (dir, filesystem.filter((file, _) => file.startsWith(dir)).map((_, size) => size).sum))
        .toMap

    val result = dirs.map((_, size) => size).filter(_ > dirs("/") - 40000000).toList.min

    println(result)
}
