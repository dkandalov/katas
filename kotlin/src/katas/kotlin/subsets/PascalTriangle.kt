package katas.kotlin.subsets

import katas.kotlin.shouldEqual
import kotlincommon.printed
import org.junit.Test

class PascalTriangle {
    @Test fun `generate Pascal triangle`() {
        pascalTriangle(9).joinToString("\n").printed() shouldEqual """
            [1]
            [1, 1]
            [1, 2, 1]
            [1, 3, 3, 1]
            [1, 4, 6, 4, 1]
            [1, 5, 10, 10, 5, 1]
            [1, 6, 15, 20, 15, 6, 1]
            [1, 7, 21, 35, 35, 21, 7, 1]
            [1, 8, 28, 56, 70, 56, 28, 8, 1]
        """.trimIndent()
    }
}

fun pascalTriangle(n: Int): List<List<Int>> {
    if (n == 1) return listOf(listOf(1))

    val results = pascalTriangle(n - 1)
    val list = results.last()
    val line = listOf(list.first()) + IntRange(1, list.size / 2).map { i -> list[i - 1] + list[i] }

    return results + listOf(line.mirrorToRight(newSize = list.size + 1))
}

private fun List<Int>.mirrorToRight(newSize: Int): List<Int> =
    when {
        size * 2 == newSize     -> this + this.reversed()
        size * 2 == newSize + 1 -> this + this.reversed().drop(1)
        else                    -> error("")
    }
