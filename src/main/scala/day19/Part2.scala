//package day19
//import scala.collection.mutable
//
//case class Blueprint(
//    blueprintId: Int,
//    oreRobotCost: Int,
//    clayRobotCost: Int,
//    obsidianRobotOreCost: Int,
//    obsidianRobotClayCost: Int,
//    geodeRobotOreCost: Int,
//    geodeRobotObsidianCost: Int
//)
//
//val cache = mutable.Map[(Int, Int), Node]()
//
//case class Node(t: Int, action: Int):
//    private var _resources = Array.fill[Int](4)(0)
//    private var _robots = Array.fill[Int](4)(0)
//    private var evaluated = false
//    private var valid = true
//
//    def isValid(blueprint: Blueprint): Boolean = {
//        if (!evaluated) {
//            evaluate(blueprint)
//        }
//
//        valid
//    }
//
//    private def getPrev(blueprint: Blueprint): Option[Node] = {
//        if (t == 0) {
//            None
//        } else {
//            (-1 to 3)
//                .map(a => (t - 1, a))
//                .map((t, x) => {
//                    if cache.contains((t, x)) then cache((t, x))
//                    else {
//                        val n = Node(t, x)
//                        cache.put((t, x), n)
//                        n
//                    }
//                })
//                .maxByOption(x => x.score(blueprint))
//        }
//    }
//
//    private def evaluate(blueprint: Blueprint): Unit = {
//        if (!evaluated) {
//            val prev = getPrev(blueprint)
//            if (!prev.forall(_.isValid(blueprint))) {
//                valid = false
//                return
//            }
//
//            val (prevResources, prevRobots) = prev match {
//                case Some(n) => (n.resources(blueprint).get, n.robots(blueprint).get)
//                case _       => (Array.fill[Int](4)(0), Array.fill[Int](4)(0))
//            }
//
//            val t0 = prev.map(_.t).getOrElse(0)
//            val dt = t - t0
//
//            _resources = prevResources.zip(prevRobots).map((resource, robots) => resource + dt * robots)
//
//            action match {
//                // Do nothing
//                case -1 =>
//                // Buy ore robot
//                case 0 =>
//                    _resources(0) -= blueprint.oreRobotCost
//                    _robots(0) += 1
//                // Buy clay robot
//                case 1 =>
//                    _resources(0) -= blueprint.clayRobotCost
//                    _robots(1) += 1
//                // Buy obsidian robot
//                case 2 =>
//                    _resources(0) -= blueprint.obsidianRobotOreCost
//                    _resources(1) -= blueprint.obsidianRobotClayCost
//                    _robots(2) += 1
//                // Buy geode robot
//                case 3 =>
//                    _resources(0) -= blueprint.geodeRobotOreCost
//                    _resources(2) -= blueprint.geodeRobotObsidianCost
//                    _robots(3) += 1
//            }
//
//            _resources.find(_ < 0) match {
//                case None =>
//                    _resources = _resources.zip(prevRobots).map(_ + _)
//                case _ =>
//                    valid = false
//            }
//        }
//    }
//
//    def resources(blueprint: Blueprint): Option[Array[Int]] = {
//        if (!evaluated) {
//            evaluate(blueprint)
//        }
//
//        if valid then Some(_resources) else None
//    }
//
//    def robots(blueprint: Blueprint): Option[Array[Int]] = {
//        if (!evaluated) {
//            evaluate(blueprint)
//        }
//
//        if valid then Some(_robots) else None
//    }
//
//    def score(blueprint: Blueprint): Option[Int] = resources(blueprint).map(_(3))
//
//@main def day19_part2() = {
//    val sep = System.lineSeparator()
//
//    val r = raw"Blueprint (\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+".r
//
//    val input = util.readInput(19, example = true)
//
//    val blueprints = input.split(sep).map {
//        case r(
//              blueprintId,
//              oreRobotCost,
//              clayRobotCost,
//              obsidianRobotOreCost,
//              obsidianRobotClayCost,
//              geodeRobotOreCost,
//              geodeRobotObsidianCost
//            ) =>
//            Blueprint(
//              blueprintId.toInt,
//              oreRobotCost.toInt,
//              clayRobotCost.toInt,
//              obsidianRobotOreCost.toInt,
//              obsidianRobotClayCost.toInt,
//              geodeRobotOreCost.toInt,
//              geodeRobotObsidianCost.toInt
//            )
//    }
//
//    val blueprint = blueprints.drop(0).head
//
//    val cache = mutable.Set[Node]()
//
//    val result = Node(10, -1).score(blueprint)
//    println(result)
//
//}
