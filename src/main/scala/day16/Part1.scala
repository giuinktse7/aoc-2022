//package day16
//import util.*
//
//import java.io.File
//import scala.collection.mutable
//import scala.math.Ordering
//
//// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
//case class Valve(id: String, flowRate: Int, connectedValves: List[String])
//
//case class Node(valve: Valve, remainingTime: Int, totalFlowRate: Int, opened: Set[String], releasedPressure: Int)
//
//@main def day16_part1() = {
//    val sep = System.lineSeparator()
//
//    val valveR = raw"Valve ([^ ]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)".r
//
//    val input = util.readInput(16, example = true)
//
//    val valves: Array[Valve] = input
//        .split(sep)
//        .map { case valveR(id, flowRate, connectedValves) =>
//            Valve(id, flowRate.toInt, connectedValves.split(",").map(_.trim).toList)
//        }
//
//    valves.foreach(println)
//
//    val start = valves.find(x => x.id == "AA").get
//
//    val valveMap = valves.map(v => (v.id, v)).toMap
//
//    // Open valve = 1min
//    // Go through tunnel = 1min
//
//    // How could I have gotten here in less than N minutes?
//    // Record the best flow given those possibilities
//
//    val b = mutable.Map[Int, mutable.Map[String, (Int, List[String], List[String])]]()
//
//    def best(valveId: String, remainingTime: Int): (Int, List[String], List[String]) = {
//        b.get(remainingTime).flatMap(_.get(valveId)) match {
//            case Some(x) => x
//            case None =>
//                if (remainingTime == 30) {
//                    if (valveId == "AA") {
//                        (0, Nil, "AA" :: Nil)
//                    } else {
//                        (0, Nil, Nil)
//                    }
//                }
//                else if (remainingTime >= 29) {
//                    if (valveMap(valveId).connectedValves.contains("AA")) {
//                        (0, Nil, valveId :: "AA" :: Nil)
//                    }
//                    else {
//                        (0, Nil, Nil)
//                    }
//                }
//                else {
//                    if (!b.contains(remainingTime)) {
//                        b += (remainingTime -> mutable.Map())
//                    }
//                    val valve = valveMap(valveId)
//
//                    val bestPrev = (valve.connectedValves :+ valveId)
//                        .map(x => best(x, remainingTime + 1))
//                        .filter((_, _, visited) =>
//                            visited.headOption.exists(valve.connectedValves.contains) && visited.lastOption
//                                .contains("AA")
//                        )
//                        .maxByOption((pressure, _, _) => pressure)
//
//                    bestPrev match {
//                        case Some((pressure, opened, visited)) =>
//                            if (opened.contains(valveId)) {
//                                val res = (pressure, opened, valveId :: visited)
//                                b(remainingTime)(valveId) = res
//                                res
//                            } else {
//                                val res = (
//                                  pressure + valve.flowRate * (remainingTime - 1),
//                                  valveId :: opened,
//                                  valveId :: opened
//                                )
//                                b(remainingTime)(valveId) = res
//                                res
//                            }
//                        case None =>
//                            val res = (0, Nil, Nil)
//                            b(remainingTime)(valveId) = res
//                            res
//                    }
//                }
//        }
//    }
//
//    valves.map(_.id).map(id => best(id, 2)).foreach { println }
//}
