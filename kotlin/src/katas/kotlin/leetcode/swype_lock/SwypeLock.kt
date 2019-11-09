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

        1.let { n ->
            listOf(2, 4, 5).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(3, 6, 7, 8, 9).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        2.let { n ->
            listOf(1, 3, 4, 5, 6).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(7, 8, 9).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        3.let { n ->
            listOf(2, 5, 6).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(1, 4, 7, 8, 9).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        4.let { n ->
            listOf(1, 2, 5, 8, 7).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(3, 6, 9).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        5.let { n ->
            listOf(1, 2, 3, 4, 6, 7, 8, 9).forEach { validate(listOf(n, it)) shouldEqual true }
        }

        6.let { n ->
            listOf(2, 3, 5, 8, 9).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(1, 4, 7).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        7.let { n ->
            listOf(4, 5, 8).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(1, 2, 3, 6, 9).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        8.let { n ->
            listOf(4, 5, 6, 7, 9).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(1, 2, 3).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        9.let { n ->
            listOf(5, 6, 8).forEach { validate(listOf(n, it)) shouldEqual true }
            listOf(1, 2, 3, 4, 7).forEach { validate(listOf(n, it)) shouldEqual false }
        }

        validate(listOf(1, 2, 3)) shouldEqual true
        validate(listOf(3, 2, 1)) shouldEqual true
        validate(listOf(1, 2, 3, 4)) shouldEqual false
        validate(listOf(4, 3, 2, 1)) shouldEqual false
    }
}

private fun validate(swypeLock: List<Int>): Boolean {
    return swypeLock.size in 1..9 &&
        swypeLock.size == swypeLock.toSet().size &&
        swypeLock.all { it in 1..9 } &&
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
