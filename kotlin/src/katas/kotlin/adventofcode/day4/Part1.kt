package katas.kotlin.adventofcode.day4

import nonstdlib.*

fun main() {
    (372304..847060).count { n ->
        val digits = n.digits.windowed(size = 2)
        digits.any { it[0] == it[1] } && digits.all { it[0] <= it[1] }
    }.printed()
}

val Int.digits: List<Int> get() = toString().toCharArray().map { it.toInt() }