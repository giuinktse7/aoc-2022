package day15
import util.*

import java.io.File
import scala.collection.immutable.::
import scala.math.Ordering

@main def day15_part2() = {
    val input = util.readInput(15)

    val sensors = parseSensors(input)
    val sideLength = 4000000

    val distressBeaconPos = sensors
        .flatMap(sensor => {
            val adjacentSensors = sensors.filter(s => s != sensor && s.adjacent(sensor))

            sensor.edge
                .filter(pos => pos.x >= 0 && pos.x <= sideLength && pos.y >= 0 && pos.y <= sideLength)
                .find(pos => !adjacentSensors.exists(_.contains(pos)))
        })
        .head

    val result = distressBeaconPos.x.toLong * sideLength.toLong + distressBeaconPos.y.toLong
    println(result)
}
