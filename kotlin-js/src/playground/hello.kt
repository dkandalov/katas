package playground

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.js.Math
import kotlin.js.Math.PI
import kotlin.js.Math.sqrt

@Suppress("unused")
fun hello(): String {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D
    fun List<Point>.display(): List<Point> {
        shift(100.0, 100.0)
            .let { points ->
                context.moveTo(points.first().x, points.first().y)
                points.forEach {
                    context.lineTo(it.x, it.y)
                    context.stroke()
                    context.moveTo(it.x, it.y)
                }
            }
        return this
    }

    val snowflakePoints = snowflakePoints()
    snowflakePoints.display()

    val h = (sqrt(3.0)/2) * 100
    val cx = 50.0
    val cy = sqrt(150.0) + (2 * (h / 3.0))
    context.rect(100 + cx, 100 + cy, 2.0, 2.0)

    snowflakePoints
        .rotate(0.0, 100.0, PI)
        .scale(0.0, 100.0, 1 / 3.0)
        .shift(2 * (50.0 - (1/3.0 * 50)), 0.0)
        .display()
        .rotate(cx, cy, 2 * PI / 3).display()
        .rotate(cx, cy, 2 * PI / 3).display()


    return "Hello from Kotlin function"
}

fun snowflakePoints(): List<Point> {
    return listOf(
        Point(0.0, 100.0),
        Point(50.0, sqrt(150.0)),
        Point(100.0, 100.0)
    )
}

data class Point(val x: Double, val y: Double) {
    fun shift(x: Double, y: Double) = Point(this.x + x, this.y + y)

    fun scale(value: Double) = Point(x * value, y * value)

    fun scale(originX: Double, originY: Double, value: Double) =
        this.shift(-originX, -originY)
            .scale(value)
            .shift(originX, originY)

    fun rotate(angle: Double): Point {
        val cos = Math.cos(angle)
        val sin = Math.sin(angle)
        return Point(
            x = (x * cos - y * sin),
            y = (x * sin + y * cos)
        )
    }

    fun rotate(originX: Double, originY: Double, angle: Double): Point =
        this.shift(-originX, -originY)
            .rotate(angle)
            .shift(originX, originY)
}

fun List<Point>.scale(x: Double, y: Double, value: Double) = map { it.scale(x, y, value) }
fun List<Point>.shift(x: Double, y: Double) = map { it.shift(x, y) }
fun List<Point>.rotate(x: Double, y: Double, angle: Double) = map { it.rotate(x, y, angle) }
