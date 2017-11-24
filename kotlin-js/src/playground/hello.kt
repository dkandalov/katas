package playground

import org.w3c.dom.get
import kotlin.browser.document
import kotlin.browser.window

fun main(args: Array<String>) {
    println(window["myCanvas"])
    console.info("an info")
    document.bgColor = "112233" // doesn't really set background
}

fun hello() = "Hello from Kotlin function"