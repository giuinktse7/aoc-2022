package day7.common

import scala.collection.immutable

case class State(files: immutable.Map[String, Long], path: String)

def parent(path: String) =
    path.stripSuffix("/").lastIndexOf("/") match {
        case -1 => "/"
        case i  => path.slice(0, i)
    }

def relative(p1: String, p2: String) = s"$p1/$p2/".replaceAll("//", "/")

def allSubPaths(path: String) = {
    path.split("/").foldLeft(List[String]()) { case (acc, next) =>
        acc :+ acc.lastOption.map(x => relative(x, next)).getOrElse(next + "/")
    }
}

def createFileSystem(input: String) = {
    val sep = System.lineSeparator()

    val cd = (raw"\$$ cd (.*)").r
    val file = raw"(\d+) ([^.]*.?.*)".r

    input
        .split(sep)
        .foldLeft(State(immutable.Map(), "")) { (state, line) =>
            line match {
                case cd(args) =>
                    args match {
                        case ".." => state.copy(path = parent(state.path))
                        case "/"  => state.copy(path = "/")
                        case dir  => state.copy(path = relative(state.path, dir))
                    }
                case file(size, file) =>
                    state.copy(files = state.files.updated(relative(state.path, file), size.toLong))

                case _ => state
            }
        }
        .files
}
