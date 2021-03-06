package katas.kotlin.leetcode.swype_lock

import datsok.*
import org.junit.*
import kotlin.math.*

/**
 * By Dmitry Buykin.
 */
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

        check(digit = 1, validMoves = listOf(2, 4, 5), invalidMoves = listOf(1, 3, 6, 7, 8, 9))
        check(digit = 2, validMoves = listOf(1, 3, 4, 5, 6), invalidMoves = listOf(2, 7, 8, 9))
        check(digit = 3, validMoves = listOf(2, 5, 6), invalidMoves = listOf(1, 3, 4, 7, 8, 9))
        check(digit = 4, validMoves = listOf(1, 2, 5, 8, 7), invalidMoves = listOf(3, 4, 6, 9))
        check(digit = 5, validMoves = listOf(1, 2, 3, 4, 6, 7, 8, 9), invalidMoves = listOf(5))
        check(digit = 6, validMoves = listOf(2, 3, 5, 8, 9), invalidMoves = listOf(1, 4, 6, 7))
        check(digit = 7, validMoves = listOf(4, 5, 8), invalidMoves = listOf(1, 2, 3, 6, 7, 9))
        check(digit = 8, validMoves = listOf(4, 5, 6, 7, 9), invalidMoves = listOf(1, 2, 3, 8))
        check(digit = 9, validMoves = listOf(5, 6, 8), invalidMoves = listOf(1, 2, 3, 4, 7, 9))

        validate(listOf(1, 2, 3)) shouldEqual true
        validate(listOf(3, 2, 1)) shouldEqual true
        validate(listOf(1, 2, 3, 4)) shouldEqual false
        validate(listOf(4, 3, 2, 1)) shouldEqual false
    }

    private fun check(digit: Int, validMoves: List<Int>, invalidMoves: List<Int>) {
        validMoves.forEach {
            validate(listOf(digit, it)) shouldEqual true
            validate(listOf(it, digit)) shouldEqual true
        }
        invalidMoves.forEach {
            validate(listOf(digit, it)) shouldEqual false
            validate(listOf(it, digit)) shouldEqual false
        }
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
