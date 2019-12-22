package katas.kotlin.adventofcode.day8

import java.io.*

fun main() {
    val digits = File("src/katas/kotlin/adventofcode/day8/input.txt").readText().map { it.toString().toInt() }
    val width = 25
    val height = 6
    val layerSize = width * height
    val transparentImage = List(layerSize) { 2 }

    val decodedImage = digits.chunked(layerSize)
        .fold(transparentImage) { image, layer ->
            image.zip(layer).map { (imagePixel, layerPixel) ->
                if (imagePixel == 2) layerPixel else imagePixel
            }
        }

    val message = decodedImage.chunked(width)
        .map { it.joinToString("").replace("0", " ") }
        .joinToString("\n")

    println(message)
}