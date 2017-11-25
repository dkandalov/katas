package playground

import org.w3c.dom.HTMLCanvasElement
import org.w3c.dom.RenderingContext
import kotlin.browser.document

fun hello(): String {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context: RenderingContext = canvas.getContext("2d")!! as RenderingContext
    context.moveTo(0, 0)
    context.lineTo(200, 100)
    context.stroke()

    return "Hello from Kotlin function"
}