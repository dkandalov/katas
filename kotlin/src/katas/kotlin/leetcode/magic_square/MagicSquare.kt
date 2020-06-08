package katas.kotlin.leetcode.magic_square

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/341295/Google-or-Online-Assessment-2019-or-Fill-Matrix
 */
class MagicSquareTests {
    @Test fun `find magic square of size n`() {
        magicSquare(size = 2) shouldEqual emptyList()
        magicSquare(size = 3) shouldEqual listOf(
            listOf(8, 3, 4),
            listOf(1, 5, 9),
            listOf(6, 7, 2)
        )
/*
        magicSquare(size = 4) shouldEqual listOf(
            listOf(8, 3, 4),
            listOf(1, 5, 9),
            listOf(6, 7, 2)
        )
*/
    }
}

private fun magicSquare(size: Int): List<List<Int>> {
    return magicSquare(emptySolution(size)).firstOrNull()?.rows() ?: emptyList()
}

private fun magicSquare(s: Solution): Sequence<Solution> {
    if (s.isComplete()) return sequenceOf(s)
    return s.nextSteps()
        .filter { it.isValid() }
        .flatMap { magicSquare(it) }
}

private fun emptySolution(size: Int) = Solution(list = emptyList(), size = size)

private data class Solution(val list: List<Int>, val size: Int) {
    private val targetSum = (1..(size * size)).sum() / 3

    fun isComplete() = list.size == size * size

    fun nextSteps() =
        ((size * size).downTo(1) - list).asSequence()
            .map { step -> copy(list = list + step) }

    fun isValid() =
        (rows() + columns() + diagonals())
            .filterNot { it.contains(0) }
            .all { it.sum() == targetSum }

    fun rows() =
        (0 until size).map { row ->
            (0 until size).map { column ->
                this[row * size + column]
            }
        }

    fun columns() =
        (0 until size).map { column ->
            (0 until size).map { row ->
                this[row * size + column]
            }
        }

    fun diagonals() =
        listOf(
            (0 until size).map { i -> this[i * size + i] },
            (0 until size).map { i -> this[i * size + (size - i - 1)] }
        )

    private operator fun get(i: Int) = if (i < list.size) list[i] else 0
}
