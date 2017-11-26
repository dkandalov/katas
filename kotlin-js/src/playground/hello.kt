package playground

import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.js.Math

@Suppress("unused")
fun hello(): String {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D
    snowflakePoints().let { points ->
        points.forEach {
            context.lineTo(it.x, it.y)
            context.stroke()
            context.moveTo(it.x, it.y)
        }
    }

    return "Hello from Kotlin function"
}

data class Point(val x: Double, val y: Double)

fun snowflakePoints(): List<Point> {
    return listOf(
        Point(0.0, 100.0),
        Point(50.0, Math.sqrt(150.0)),
        Point(100.0, 100.0)
    )
}
