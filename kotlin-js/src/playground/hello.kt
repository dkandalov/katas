package playground

import kotlin.browser.document

fun main(args: Array<String>) {
    println("hello")
    console.info("an info")
    document.bgColor = "112233" // doesn't really set background
}

fun hello() = "Hello from Kotlin function"