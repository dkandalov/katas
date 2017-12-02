package lsystem

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.coroutines.experimental.buildSequence
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin


@Suppress("unused")
@JsName("drawSnowflake2")
fun main() {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D

    val points = `Fractal plant`
        .generatePoints(stepLength = 10.0, depth = 9)
        .toList()

    context.beginPath()
    points.fitInto(canvas)
        .zipWithNext()
        .forEach { (p1, p2) ->
            if (p1 != Point.none && p2 != Point.none) {
                context.moveTo(p1.x, p1.y)
                context.lineTo(p2.x, p2.y)
            }
        }
    context.closePath()
    context.stroke()
}

val kochSnowflake = LSystem(
    start = "F--F--F",
    rules = mapOf('F' to "F+F--F+F"),
    angle = PI / 3,
    closedPath = true
)

private val `Cesaro fractal` = LSystem(
    start = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = 85.toRadians()
)

// TODO http://mathworld.wolfram.com/CesaroFractal.html
private val `Cesaro fractal 2` = LSystem(
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

// https://en.wikipedia.org/wiki/Hilbert_curve
private val `Hilber curve` = LSystem(
    start = "A",
    rules = mapOf(
        'A' to "-BF+AFA+FB-",
        'B' to "+AF-BFB-FA+"
    ),
    angle = PI / 2
)

// https://en.wikipedia.org/wiki/Gosper_curve
private val `Gosper curve` = LSystem(
    start = "F",
    rules = mapOf(
        'F' to "F-G--G+F++FF+G-",
        'G' to "+F-GG--G-F++F+G"
    ),
    angle = 60.toRadians()
)

// https://en.wikipedia.org/wiki/Sierpinski_triangle
private val `Sierpinski triangle` = LSystem(
    start = "F-G-G",
    rules = mapOf(
        'F' to "F-G+F+G-F",
        'G' to "GG"
    ),
    angle = 120.toRadians()
)

/*private val pentaFlake = LSystem(
    start = "F-F-F-F-F",
    rules = mapOf(
        'F' to "F[-F-F-F-F]"*//*,
        'G' to "F[-F-F-F-F]"*//*
    ),
    angle = PI / 2.5
)

private val pentaFlake0 = LSystem(
    start = "F-F-F-F-F",
    rules = mapOf(
        'F' to "F-F[-F-F-F-F]----F",
        'G' to ""
    ),
    angle = PI / 2.5
)*/

// https://en.wikipedia.org/wiki/Sierpi%C5%84ski_arrowhead_curve
private val `Sierpinski arrowhead curve` = LSystem(
    start = "F",
    rules = mapOf(
        'F' to "G-F-G",
        'G' to "F+G+F"
    ),
    angle = PI / 3,
    initialAngle = PI
)

// https://en.wikipedia.org/wiki/Dragon_curve
private val `Dragon curve` = LSystem(
    start = "FX",
    rules = mapOf(
        'X' to "X+YF+",
        'Y' to "-FX-Y"
    ),
    angle = PI / 2,
    initialAngle = 1.5 * PI
)

private val `Fractal plant` = LSystem(
    start = "X",
    rules = mapOf(
        'X' to "F[-X][X]F[-X]+FX",
        'F' to "FF"
    ),
    angle = 25.toRadians(),
    initialAngle = -PI / 2
)

// From http://www.cs.unh.edu/~charpov/programming-lsystems.html
private val `Fractal plant 2` = LSystem(
    start = "F",
    rules = mapOf('F' to "FF-[-F+F+F]+[+F-F-F]"),
    angle = 22.5.toRadians(),
    initialAngle = -PI / 2
)


class LSystem(
    private val start: String,
    private val rules: Map<Char, String>,
    private val angle: Double,
    private val initialAngle: Double = 0.0,
    private val closedPath: Boolean = false
) {
    fun generatePoints(stepLength: Double = 10.0, depth: Int = 3): Sequence<Point> {
        return generateOutput(start, depth).toPoints(stepLength)
    }

    private fun generateOutput(input: String, depth: Int): String {
        if (depth == 0) return input
        val result = input
            .asIterable()
            .joinToString("") { c -> rules[c] ?: c.toString() }
        return generateOutput(result, depth - 1)
    }

    private fun String.toPoints(stepLength: Double): Sequence<Point> {
        return buildSequence {
            val startPoint = Point(0.0, 0.0)
            yield(startPoint)

            var angle = initialAngle
            var p = startPoint
            val stack = ArrayList<Pair<Point, Double>>()
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
                    '[' -> stack.add(0, Pair(p, angle))
                    ']' -> {
                        val removed = stack.removeAt(0)
                        p = removed.first
                        angle = removed.second
                        yield(Point.none)
                    }
                }
            }
            if (closedPath) yield(startPoint)
        }
    }
}

fun Double.toRadians(): Double = (this / 180.0) * PI
fun Int.toRadians(): Double = toDouble().toRadians()
