package lsystem

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.math.atan2
import kotlin.math.sqrt

@Suppress("unused")
@JsName("drawSnowflake")
fun drawSnowflake() {
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

    kochSnowflakePoints().display()
}

private fun kochSnowflakePoints(length: Double = 300.0): List<Point> {
    val h = (sqrt(3.0) / 2) * length
    val points = listOf(
        Point(0.0, h),
        Point(length, h),
        Point(length / 2, 0.0)
    )
    return addNested(points, length) + points.first()
}

private fun addNested(points: List<Point>, length: Double, depth: Int = 1): List<Point> {
    if (depth == 0) return points
    
    val result = points.pairs()
        .flatMap { (p1, p2) ->
            val angle = atan2(p2.y - p1.y, p2.x - p1.x)
            val h = (sqrt(3.0) / 2) * length
            listOf(
                Point(x = 0.0, y = 0.0),
                Point(x = 1.0 / 3 * length, y = 0.0),
                Point(x = 1.5 / 3 * length, y = h / 3),
                Point(x = 2.0 / 3 * length, y = 0.0)
            ).map {
                it.rotate(angle).shift(p1.x, p1.y)
            }
        }
    return addNested(result, length / 3, depth - 1)
}

fun <T> List<T>.pairs(): List<Pair<T, T>> = (this + first()).zipWithNext()
