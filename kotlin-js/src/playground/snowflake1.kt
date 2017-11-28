package playground

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin


@Suppress("unused")
@JsName("drawSnowflake2")
fun drawSnowflake2() {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D

    val length = 5
    var angle = 0.0
    var p = Point(200.0, 200.0)
    snowflakes().forEach { c ->
        when (c) {
            'F' -> {
                val p2 = Point(
                    x = p.x + (cos(angle) * length),
                    y = p.y + (sin(angle) * length)
                )
                context.moveTo(p.x, p.y)
                context.lineTo(p2.x, p2.y)
                context.stroke()
                p = p2
            }
            '+' -> angle += PI / 3
            '-' -> angle -= PI / 3
        }
    }
}

fun snowflakes(input: String = "F--F--F", depth: Int = 3): String {
    if (depth == 0) return input
    val result = input
        .map { c -> rules[c] ?: c }
        .joinToString("")
    return snowflakes(result, depth - 1)
}

val rules = mapOf('F' to "F+F--F+F")

