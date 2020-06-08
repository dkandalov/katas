package katas.kotlin.adventofcode.day1

import nonstdlib.*
import java.io.*
import kotlin.math.*

fun main() {
    File("src/katas/kotlin/adventofcode/day1/input.txt")
        .readLines().map { it.toInt() }
        .map { calcFuel(it) }
        .sum().printed()
}

private fun calcFuel(it: Int): Int {
    val result = floor(it / 3.0).toInt() - 2
    return if (result <= 0) 0 else result + calcFuel(result)
}