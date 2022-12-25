package day15
import util.*
import java.io.File
import scala.math.Ordering

@main def day15_part1() = {

    val input = util.readInput(15)

    val sensors = parseSensors(input)

    val y0 = 2000000

    val result = sensors
        .filter(_.intersectsY(y0))
        .flatMap(sensor => {
            val d = sensor.range - (sensor.pos.y - y0).abs
            val r = (sensor.pos.x - d) to (sensor.pos.x + d)
            r.filter(x => Pos(x, y0) != sensor.closestBeaconPos)
        })
        .distinct

    println(result.length)
}
