package katas.kotlin.adventofcode.day1

import kotlincommon.*
import java.io.*
import kotlin.math.*

fun main() {
    File("src/katas/kotlin/adventofcode/day1/input.txt")
        .readLines()
        .map { it.toInt() }
        .map { floor(it / 3.0).toInt() - 2 }
        .sum().printed()
}