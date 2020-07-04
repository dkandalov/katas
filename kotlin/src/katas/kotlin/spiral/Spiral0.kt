package katas.kotlin.spiral

import katas.kotlin.softFail
import nonstdlib.joinBy
import org.junit.jupiter.api.Test


/**
 * Write a function that returns a spiral.
 * Input size of the spiral won’t be smaller than 5.
 *
 * Input: 5
 * Output:
 * {
 * {1,1,1,1,1},
 * {0,0,0,0,1},
 * {1,1,1,0,1},
 * {1,0,0,0,1},
 * {1,1,1,1,1},
 * }
 *
 * Input: 8
 * Output:
 * {
 * {1,1,1,1,1,1,1,1},
 * {0,0,0,0,0,0,0,1},
 * {1,1,1,1,1,1,0,1},
 * {1,0,0,0,0,1,0,1},
 * {1,0,1,0,0,1,0,1},
 * {1,0,1,1,1,1,0,1},
 * {1,0,0,0,0,0,0,1},
 * {1,1,1,1,1,1,1,1},
 * }
 */
class Spiral0 {
    @Test fun `it works`() = softFail {
        spiral(size = 3).toPrintableString() shouldEqual """
            X X X
            - - X
            X X X
        """.trimIndent()

        spiral(size = 4).toPrintableString() shouldEqual """
            X X X X
            - - - X
            X - - X
            X X X X
        """.trimIndent()

        spiral(size = 5).toPrintableString() shouldEqual """
            X X X X X
            - - - - X
            X X X - X
            X - - - X
            X X X X X
        """.trimIndent()

        spiral(size = 6).toPrintableString() shouldEqual """
            X X X X X X
            - - - - - X
            X X X X - X
            X - - X - X
            X - - - - X
            X X X X X X
        """.trimIndent()

        spiral(size = 7).toPrintableString() shouldEqual """
            X X X X X X X
            - - - - - - X
            X X X X X - X
            X - - - X - X
            X - X X X - X
            X - - - - - X
            X X X X X X X
        """.trimIndent()

        spiral(size = 8).toPrintableString() shouldEqual """
            X X X X X X X X
            - - - - - - - X
            X X X X X X - X
            X - - - - X - X
            X - X - - X - X
            X - X X X X - X
            X - - - - - - X
            X X X X X X X X
        """.trimIndent()

        spiral(size = 9).toPrintableString() shouldEqual """
            X X X X X X X X X
            - - - - - - - - X
            X X X X X X X - X
            X - - - - - X - X
            X - X X X - X - X
            X - X - - - X - X
            X - X X X X X - X
            X - - - - - - - X
            X X X X X X X X X
        """.trimIndent()

        spiral(size = 10).toPrintableString() shouldEqual """
            X X X X X X X X X X
            - - - - - - - - - X
            X X X X X X X X - X
            X - - - - - - X - X
            X - X X X X - X - X
            X - X - - X - X - X
            X - X - - - - X - X
            X - X X X X X X - X
            X - - - - - - - - X
            X X X X X X X X X X
        """.trimIndent()

        spiral(size = 11).toPrintableString() shouldEqual """
            X X X X X X X X X X X
            - - - - - - - - - - X
            X X X X X X X X X - X
            X - - - - - - - X - X
            X - X X X X X - X - X
            X - X - - - X - X - X
            X - X - X X X - X - X
            X - X - - - - - X - X
            X - X X X X X X X - X
            X - - - - - - - - - X
            X X X X X X X X X X X
        """.trimIndent()
    }
}

private fun List<List<Int>>.toPrintableString() =
    joinBy("\n") { line ->
        line.joinBy(" ") {
            if (it == 0) "-" else "X"
        }
    }

fun spiral(size: Int): List<List<Int>> {
    require(size >= 0)
    val board = MutableList(size) { MutableList(size) { 0 } }
    spiral(xs = 0..size - 1, ys = 0..size - 1)
        .forEach { (x, y) -> board[x][y] = 1 }
    return board
}

fun spiral(xs: IntRange, ys: IntRange, drawTopRightCorner: Boolean = true): Sequence<Pair<Int, Int>> = sequence {
    if (ys.last <= 0) yieldAll(emptyList())
    else if (drawTopRightCorner) {
        yieldAll(xs.map { ys.first to it }) // top-left to top-right
        yieldAll(ys.map { it to xs.last }) // top-right to bottom-right
        if (xs.size > 2) yield(ys.last to xs.last - 1)
        yieldAll(spiral(xs.shiftEnd(-2), ys.shiftStart(2), !drawTopRightCorner))
    } else {
        yieldAll(xs.map { ys.last to it }) // bottom-left to bottom-right
        yieldAll(ys.map { it to xs.first }) // top-left to bottom-left
        if (xs.size > 2) yield(ys.first to xs.first + 1)
        yieldAll(spiral(xs.shiftStart(2), ys.shiftEnd(-2), !drawTopRightCorner))
    }
}

fun spiral_(
    board: MutableList<MutableList<Int>>,
    xs: IntRange,
    ys: IntRange,
    drawTopRightCorner: Boolean = true
): List<List<Int>> {
    if (ys.last <= 0) return board
    return if (drawTopRightCorner) {
        xs.forEach { board[ys.first][it] = 1 } // top-left to top-right
        ys.forEach { board[it][xs.last] = 1 } // top-right to bottom-right
        if (xs.size > 2) board[ys.last][xs.last - 1] = 1
        spiral_(board, xs.shiftEnd(-2), ys.shiftStart(2), !drawTopRightCorner)
    } else {
        xs.forEach { board[ys.last][it] = 1 } // bottom-left to bottom-right
        ys.forEach { board[it][xs.first] = 1 } // top-left to bottom-left
        if (xs.size > 2) board[ys.first][xs.first + 1] = 1
        spiral_(board, xs.shiftStart(2), ys.shiftEnd(-2), !drawTopRightCorner)
    }
}

private fun IntRange.shiftStart(n: Int) =
    IntRange(start = start + n, endInclusive = endInclusive)

private fun IntRange.shiftEnd(n: Int) =
    IntRange(start = start, endInclusive = endInclusive + n)

private val IntRange.size: Int
    get() = endInclusive - start + 1

fun spiral_imperative(size: Int): List<List<Int>> {
    require(size >= 5)

    val result = MutableList(size) { MutableList(size) { 0 } }
    var left = 0
    var right = size - 1
    var top = 0
    var bottom = size - 1

    IntRange(left, right).forEach { result[top][it] = 1 } // left-right

    while (left < right && top < bottom) {
        IntRange(top, bottom).forEach { result[it][right] = 1 } // top-bottom
        if (bottom - top > 1) IntRange(left, right).forEach { result[bottom][it] = 1 } // right-left
        top += 2
        right -= 2

        IntRange(top, bottom).forEach { result[it][left] = 1 } // bottom-top
        if (bottom - top > 1) IntRange(left, right).forEach { result[top][it] = 1 } // left-right
        left += 2
        bottom -= 2
    }

    return result
}