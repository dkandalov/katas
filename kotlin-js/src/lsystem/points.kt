package lsystem

import org.w3c.dom.Window
import kotlin.math.cos
import kotlin.math.min
import kotlin.math.sin

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

    companion object {
        val none = Point(Double.NaN, Double.NaN)
    }
}

fun List<Point>.scale(value: Double) = map { it.scale(value) }
fun List<Point>.shift(x: Double, y: Double) = map { it.shift(x, y) }
fun Sequence<Point>.shift(x: Double, y: Double) = map { it.shift(x, y) }

fun List<Point>.fitCenteredInto(x1: Double, y1: Double, x2: Double, y2: Double): List<Point> {
    require(x1 < x2 && y1 < y2)
    val width = x2 - x1
    val height = y2 - y1

    val minPoint = Point(minBy{ it.x }!!.x, minBy{ it.y }!!.y)
    val maxPoint = Point(maxBy{ it.x }!!.x, maxBy{ it.y }!!.y)
    val pointsWidth = maxPoint.x - minPoint.x
    val pointsHeight = maxPoint.y - minPoint.y
    val minScale = min(width / pointsWidth, height / pointsHeight)

    return this
        .scale(minScale)
        .shift(
            x1 - minPoint.x * minScale + (width - pointsWidth * minScale) / 2,
            y1 - minPoint.y * minScale + (height - pointsHeight * minScale) / 2
        )
}

fun List<Point>.fitCenteredInto(window: Window, marginPercent: Double = 0.1): List<Point> {
    val margin = min(window.innerWidth * marginPercent, window.innerHeight * marginPercent)
    return fitCenteredInto(
        x1 = margin,
        y1 = margin,
        x2 = window.innerWidth.toDouble() - margin,
        y2 = window.innerHeight.toDouble() - margin
    )
}