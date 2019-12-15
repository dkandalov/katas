package katas.kotlin.adventofcode.day4

import kotlincommon.*

fun main() {
    (372304..847060).count(::isValidPassword).printed()
}

fun isValidPassword(n: Int): Boolean {
    val digits = n.digits
    val digitGroups = digits.fold(emptyList<MutableList<Int>>()) { acc, it ->
        if (acc.lastOrNull()?.lastOrNull() == it) {
            acc.last().add(it)
            acc
        } else acc + listOf(mutableListOf(it))
    }
    return digitGroups.any { it.size == 2 } && digits.windowed(size = 2).all { it[0] <= it[1] }
}


