package katas.kotlin.leetcode.swype_lock

import kotlincommon.test.*
import org.junit.*
import kotlin.math.*

class SwypeLock {
    @Test fun `some examples`() {
        // 1 2 3
        // 4 5 6
        // 7 8 9

        validate(emptyList()) shouldEqual false
        validate((1..10).toList()) shouldEqual false
        validate(listOf(1, 1)) shouldEqual false
        validate(listOf(0)) shouldEqual false
        validate(listOf(10)) shouldEqual false

        validate(listOf(1, 2)) shouldEqual true
        validate(listOf(1, 3)) shouldEqual false
        validate(listOf(1, 4)) shouldEqual true
        validate(listOf(1, 5)) shouldEqual true

        validate(listOf(2, 1)) shouldEqual true
        validate(listOf(2, 3)) shouldEqual true
        validate(listOf(2, 4)) shouldEqual true
        validate(listOf(2, 5)) shouldEqual true
        validate(listOf(2, 6)) shouldEqual true
        validate(listOf(2, 7)) shouldEqual false
        validate(listOf(2, 8)) shouldEqual false
        validate(listOf(2, 9)) shouldEqual false

        validate(listOf(3, 1)) shouldEqual false
        validate(listOf(3, 2)) shouldEqual true
        validate(listOf(3, 4)) shouldEqual false
        validate(listOf(3, 5)) shouldEqual true
        validate(listOf(3, 6)) shouldEqual true
        validate(listOf(3, 7)) shouldEqual false
        validate(listOf(3, 8)) shouldEqual false
        validate(listOf(3, 9)) shouldEqual false

        listOf(1, 2, 5, 8, 7).forEach { validate(listOf(4, it)) shouldEqual true }
        listOf(3, 6, 9).forEach { validate(listOf(4, it)) shouldEqual false }

        listOf(1, 2, 3, 4, 6, 7, 8, 9).forEach { validate(listOf(5, it)) shouldEqual true }

        validate(listOf(1, 2, 3)) shouldEqual true
        validate(listOf(3, 2, 1)) shouldEqual true

        validate(listOf(3, 4)) shouldEqual false
        validate(listOf(4, 3)) shouldEqual false
    }
}

private fun validate(swypeLock: List<Int>): Boolean {
    if (swypeLock.size !in 1..9 || swypeLock.distinct().size != swypeLock.size) return false
    return swypeLock.all { it in 1..9 } &&
        swypeLock.windowed(size = 2).all { (digit1, digit2) -> areNeighbours(digit1, digit2) }
}

private fun areNeighbours(digit1: Int, digit2: Int): Boolean {
    val absDiff = (digit1 - digit2).absoluteValue
    val isHorizontal = absDiff == 1 && digit1.row == digit2.row
    val isVertical = absDiff == 3
    val isDiagonal = (absDiff == 2 || absDiff == 4) && (digit1.row - digit2.row).absoluteValue == 1
    return isHorizontal || isVertical || isDiagonal
}

private val Int.row: Int get() = (this - 1) / 3
