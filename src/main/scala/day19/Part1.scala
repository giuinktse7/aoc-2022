package day19
import day19.Action.{BuildOreRobot, BuildClayRobot, BuildObsidianRobot, BuildGeodeRobot}
import scala.util.chaining.scalaUtilChainingOps

import scala.collection.mutable
import util.*

import java.io.File
import scala.math.Ordering

case class Blueprint(
    blueprintId: Int,
    oreRobotCost: Int,
    clayRobotCost: Int,
    obsidianRobotOreCost: Int,
    obsidianRobotClayCost: Int,
    geodeRobotOreCost: Int,
    geodeRobotObsidianCost: Int
):
    val maxOreCost: Int = Seq(oreRobotCost, clayRobotCost, obsidianRobotOreCost, geodeRobotOreCost).max

enum Action(val mask: Int):
    case BuildOreRobot extends Action(1)
    case BuildClayRobot extends Action(1 << 1)
    case BuildObsidianRobot extends Action(1 << 2)
    case BuildGeodeRobot extends Action(1 << 3)

case class State(
    var t: Int = 0,
    var tMax: Int = 0,
    var ore: Int = 0,
    var clay: Int = 0,
    var obsidian: Int = 0,
    var geode: Int = 0,
    var oreRobots: Int = 1,
    var clayRobots: Int = 0,
    var obsidianRobots: Int = 0,
    var geodeRobots: Int = 0,
    var bannedActions: Int = 0
):
    def possibleActions(blueprint: Blueprint): Int = {
        var res: Int = 0
        if (ore >= blueprint.geodeRobotOreCost && obsidian >= blueprint.geodeRobotObsidianCost) {
            return BuildGeodeRobot.mask
        }

        val dt = tMax - t

        val skipOre = dt > 0 && (oreRobots - blueprint.maxOreCost) + ore / dt > 1
        val skipClay = dt > 0 && (clayRobots - blueprint.obsidianRobotClayCost) + clay / dt > 1
        val skipObsidian = dt > 0 && (obsidianRobots - blueprint.geodeRobotObsidianCost) + obsidian / dt > 1

        if (
          !skipObsidian && ore >= blueprint.obsidianRobotOreCost && clay >= blueprint.obsidianRobotClayCost && obsidianRobots < blueprint.geodeRobotObsidianCost
        ) {
            res |= BuildObsidianRobot.mask
        }

        if (!skipClay && ore >= blueprint.clayRobotCost && clayRobots < blueprint.obsidianRobotClayCost) {
            res |= BuildClayRobot.mask
        }

        if (!skipOre && ore >= blueprint.oreRobotCost && oreRobots < blueprint.maxOreCost) {
            res |= BuildOreRobot.mask
        }

        res & ~bannedActions
    }

    def step(blueprint: Blueprint, action: Option[Action] = None) = {
        val f = action.map {
            case BuildOreRobot =>
                ore -= blueprint.oreRobotCost
                () => oreRobots += 1

            case BuildClayRobot =>
                ore -= blueprint.clayRobotCost
                () => clayRobots += 1

            case BuildObsidianRobot =>
                ore -= blueprint.obsidianRobotOreCost
                clay -= blueprint.obsidianRobotClayCost
                () => obsidianRobots += 1

            case BuildGeodeRobot =>
                ore -= blueprint.geodeRobotOreCost
                obsidian -= blueprint.geodeRobotObsidianCost
                () => geodeRobots += 1
        }

        t += 1
        ore += oreRobots
        clay += clayRobots
        obsidian += obsidianRobots
        geode += geodeRobots

        f.foreach(_())

        this
    }

    def maxPossible: Int = {
        val n = tMax - t
        val k = n * (n + 1) / 2 + geode * n
        geode + k
    }

def run(blueprint: Blueprint, tMax: Int) = {
    val s0 = State(tMax = tMax)
    var best: Int = 0

    val q = mutable.PriorityQueue.empty[State](Ordering.by(_.geode))
    q.addOne(s0)

    while (q.nonEmpty) {
        val state = q.dequeue()

        if (state.maxPossible >= best) {
            var actions = state.possibleActions(blueprint)
            while (actions == 0 && state.t < tMax) {
                state.step(blueprint)
                actions = state.possibleActions(blueprint) & ~state.bannedActions
            }

            if (state.geode >= best) {
                best = state.geode
            }

            if (state.t < tMax) {
                val states = Action.values
                    .filter(a => (actions & a.mask) != 0)
                    .map(a => state.copy(bannedActions = 0).step(blueprint, Some(a)))
                    .toList

                val doNothing = state
                    .copy(bannedActions = actions)
                    .step(blueprint, None)

                q.addAll(states :+ doNothing)
            }
        }
    }

    (blueprint.blueprintId, best)
}

@main def day19_part1() = {
    val sep = System.lineSeparator()

    val r = raw"Blueprint (\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+".r

    val input = util.readInput(19)

    val blueprints = input
        .split(sep)
        .map {
            case r(
                  blueprintId,
                  oreRobotCost,
                  clayRobotCost,
                  obsidianRobotOreCost,
                  obsidianRobotClayCost,
                  geodeRobotOreCost,
                  geodeRobotObsidianCost
                ) =>
                Blueprint(
                  blueprintId.toInt,
                  oreRobotCost.toInt,
                  clayRobotCost.toInt,
                  obsidianRobotOreCost.toInt,
                  obsidianRobotClayCost.toInt,
                  geodeRobotOreCost.toInt,
                  geodeRobotObsidianCost.toInt
                )
        }
        .toList

    // Part 1
    val result1 = time { blueprints.map(x => x.pipe(run(_, 24)).pipe(_ * _)).sum }
    println(s"Part 1: $result1")

    // Part 2
    val result2 = blueprints.take(3).map(x => time { x.pipe(run(_, 32)).pipe((_, y) => y) }).product
    println(s"Part 2: $result2")
}
