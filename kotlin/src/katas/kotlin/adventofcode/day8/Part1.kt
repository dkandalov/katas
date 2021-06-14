package katas.kotlin.adventofcode.day8

import java.io.*

fun main() {
    val digits = File("src/katas/kotlin/adventofcode/day8/input.txt").readText().map { it.toString().toInt() }
    val width = 25
    val height = 6
    val layerSize = width * height

    val layer = digits.chunked(layerSize)
        .minByOrNull { layer: List<Int> -> layer.count { it == 0 } }!!

    println(layer)
    println(layer.count { it == 1 } * layer.count { it == 2 })
}