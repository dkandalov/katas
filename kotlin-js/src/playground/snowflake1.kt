package playground

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.coroutines.experimental.buildSequence
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin


@Suppress("unused")
@JsName("drawSnowflake2")
fun drawSnowflake2() {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D

    val points = `Sierpinski triangle`
        .generatePoints(stepLength = 10.0, depth = 6)
        .toList()

    context.beginPath()
    points.fitInto(canvas)
        .zipWithNext().forEach { (p1, p2) ->
            context.moveTo(p1.x, p1.y)
            context.lineTo(p2.x, p2.y)
        }
    context.closePath()
    context.stroke()
}

private val `Koch snowflake` = LSystem(
    start = "F--F--F",
    rules = mapOf('F' to "F+F--F+F"),
    angle = PI / 3,
    closedPath = true
)

private val `Cesàro fractal` = LSystem(
    start = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = 85.toRadians()
)

// TODO http://mathworld.wolfram.com/CesaroFractal.html
private val `Cesàro fractal 2` = LSystem(
    start = "F",
    rules = mapOf('F' to "F+F--F+F"),
    angle = PI / 3
)

private val `Quadratic type 1 curve` = LSystem(
    start = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = PI / 2
)

private val `Quadratic type 2 curve` = LSystem(
    start = "F",
    rules = mapOf('F' to "F+F-F-FF+F+F-F"),
    angle = PI / 2
)

private val `Sierpinski triangle` = LSystem(
    start = "F-G-G",
    rules = mapOf(
        'F' to "F-G+F+G-F",
        'G' to "GG"
    ),
    angle = 120.toRadians()
)

class LSystem(
    private val start: String,
    private val rules: Map<Char, String>,
    private val angle: Double,
    private val initialAngle: Double = 0.0,
    private val closedPath: Boolean = false
) {
    fun generatePoints(stepLength: Double = 10.0, depth: Int = 3): Sequence<Point> {
        return snowflake(start, depth).toPoints(stepLength)
    }

    private fun snowflake(input: String, depth: Int): String {
        if (depth == 0) return input
        val result = input
            .asIterable()
            .joinToString { c -> rules[c] ?: c.toString() }
        return snowflake(result, depth - 1)
    }

    private fun String.toPoints(stepLength: Double): Sequence<Point> {
        return buildSequence {
            val startPoint = Point(0.0, 0.0)
            yield(startPoint)

            var angle = initialAngle
            var p = startPoint
            forEach { c ->
                when (c) {
                    'F', 'G' -> {
                        p = p.shift(
                            x = cos(angle) * stepLength,
                            y = sin(angle) * stepLength
                        )
                        yield(p)
                    }
                    '+' -> angle += this@LSystem.angle
                    '-' -> angle -= this@LSystem.angle
                }
            }
            if (closedPath) yield(startPoint)
        }
    }
}

fun Double.toRadians(): Double = (this / 180.0) * PI
fun Int.toRadians(): Double = toDouble().toRadians()
