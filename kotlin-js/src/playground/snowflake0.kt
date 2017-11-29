package playground

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.math.*

@Suppress("unused")
@JsName("drawSnowflake")
fun drawSnowflake() {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D
    fun List<Point>.display(): List<Point> {
        this.shift(100.0, 100.0)
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

    snowflakePoints().display()
}

fun snowflakePoints(length: Double = 300.0): List<Point> {
    val h = (sqrt(3.0) / 2) * length
    val points = listOf(
        Point(0.0, h),
        Point(length, h),
        Point(length / 2, 0.0)
    )
    return addNested(points, length) + points.first()
}

fun addNested(points: List<Point>, length: Double, depth: Int = 3): List<Point> {
    if (depth == 0) return points
    val result = points.pairs()
        .flatMap { (p1, p2) ->
            val angle = atan2(p2.y - p1.y, p2.x - p1.x)
            val h = (sqrt(3.0) / 2) * length
            listOf(
                Point(0.0, 0.0),
                Point(1.0 / 3 * length, 0.0),
                Point(1.5 / 3 * length, h / 3),
                Point(2.0 / 3 * length, 0.0)
            ).map {
                it.rotate(angle).shift(p1.x, p1.y)
            }
        }
    return addNested(result, length / 3, depth - 1)
}

fun <T> List<T>.pairs(): List<Pair<T, T>> = (this + first()).zipWithNext()

data class Point(val x: Double, val y: Double) {
    fun rotate(angle: Double): Point {
        val cos = cos(angle)
        val sin = sin(angle)
        return Point(
            x = x * cos - y * sin,
            y = x * sin + y * cos
        )
    }

    fun shift(x: Double, y: Double) = Point(this.x + x, this.y + y)

    fun scale(value: Double) = Point(x * value, y * value)
}

fun List<Point>.scale(value: Double) = map { it.scale(value) }
fun List<Point>.shift(x: Double, y: Double) = map { it.shift(x, y) }
fun Sequence<Point>.shift(x: Double, y: Double) = map { it.shift(x, y) }

fun List<Point>.fitInto(canvas: HTMLCanvasElement): List<Point> {
    val minPoint = Point(minBy{ it.x }!!.x, minBy{ it.y }!!.y)
    val maxPoint = Point(maxBy{ it.x }!!.x, maxBy{ it.y }!!.y)
    val xScale = canvas.width / (maxPoint.x - minPoint.x)
    val yScale = canvas.height / (maxPoint.y - minPoint.y)
    return this
        .shift(-minPoint.x, -minPoint.y)
        .scale(min(xScale, yScale))
}
