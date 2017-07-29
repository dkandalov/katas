package katas.kotlin.diamond

import katas.kotlin.shouldEqual
import katas.kotlin.tail
import org.junit.Test

class Diamond4 {
    @Test fun `diamonds of various sizes`() {
        diamond(from = 'A', to = 'A') shouldEqual "A"
        diamond(from = 'A', to = 'B') shouldEqual """
         |-A-
         |B-B
         |-A-
        """.trimMargin()
        diamond(from = 'A', to = 'C') shouldEqual """
         |--A--
         |-B-B-
         |C---C
         |-B-B-
         |--A--
        """.trimMargin()
    }

    private fun diamond(from: Char, to: Char): String {
        val size = to - from + 1
        return 0.until(size)
            .map { i ->
                val leftPad = size - (i + 1)
                val rightPad = i
                ("-" * leftPad) + (from + i) + ("-" * rightPad)
            }
            .map { it.mirrored() }
            .mirrored()
            .joinToString("\n")
    }

    private operator fun String.times(size: Int) = 0.until(size).map { this }.joinToString("")

    private fun String.mirrored() = this + this.reversed().tail()

    private fun <E> List<E>.mirrored() = this + this.reversed().tail()
}
