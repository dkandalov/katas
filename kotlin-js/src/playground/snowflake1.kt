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

    KochSnowflake.generatePoints(stepLength = 5.0)
        .shift(200.0, 200.0)
        .zipWithNext()
        .forEach { (p1, p2) ->
            context.moveTo(p1.x, p1.y)
            context.lineTo(p2.x, p2.y)
            context.stroke()
        }
}

object KochSnowflake{
    val axiom = "F--F--F"
    val rules = mapOf('F' to "F+F--F+F")
    val angleIncrement = PI / 3

    fun generatePoints(stepLength: Double = 10.0, depth: Int = 3): Sequence<Point> {
        return snowflake(axiom, depth).toPoints(stepLength)
    }

    fun snowflake(input: String, depth: Int): String {
        if (depth == 0) return input
        val result = input
            .map { c -> rules[c] ?: c }
            .joinToString("")
        return snowflake(result, depth - 1)
    }

    private fun String.toPoints(stepLength: Double): Sequence<Point> {
        return buildSequence {
            val startPoint = Point(0.0, 0.0)
            yield(startPoint)

            var angle = 0.0
            var p = startPoint
            forEach { c ->
                when (c) {
                    'F' -> {
                        p = p.shift(
                            x = cos(angle) * stepLength,
                            y = sin(angle) * stepLength
                        )
                        yield(p)
                    }
                    '+' -> angle += angleIncrement
                    '-' -> angle -= angleIncrement
                }
            }
            yield(startPoint)
        }
    }
}

