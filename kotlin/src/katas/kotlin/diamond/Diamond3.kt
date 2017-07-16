package katas.kotlin.diamond

import katas.kotlin.shouldEqual
import org.junit.Test

class Diamond3 {
    @Test fun `draw a diamond of specified size`() {
        diamond(size = 0) shouldEqual ""
        diamond(size = 1) shouldEqual "A"

        diamond(size = 2) shouldEqual """
            |-A-
            |B-B
            |-A-
        """.trimMargin()

        diamond(size = 3) shouldEqual """
            |--A--
            |-B-B-
            |C---C
            |-B-B-
            |--A--
        """.trimMargin()

        diamond(size = 4) shouldEqual """
            |---A---
            |--B-B--
            |-C---C-
            |D-----D
            |-C---C-
            |--B-B--
            |---A---
        """.trimMargin()
    }

    private fun diamond(size: Int, startLetter: Char = 'A'): String {
        return startLetter.until(startLetter + size).withIndex()
            .map { (i, c) ->
                ('-' * (size - i - 1)) + c + ('-' * i)
            }
            .map { it.reflectRight() }
            .reflectDown()
            .joinToString("\n")
    }

    private operator fun Char.times(n: Int): String {
        var s = ""
        0.until(n).forEach { s += this }
        return s
    }

    private fun String.reflectRight() = this + this.reversed().substring(1)

    private fun <E> List<E>.reflectDown() = this + this.reversed().drop(1)
}
