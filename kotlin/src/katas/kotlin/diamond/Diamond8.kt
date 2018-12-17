package katas.kotlin.diamond

import kotlincommon.test.shouldEqual
import org.junit.Test

class Diamond8 {

    @Test fun `build a text diamond`() {
        diamond(from = 'A', to = 'A') shouldEqual "A"
        diamond(from = 'A', to = 'B') shouldEqual
            "-A-\n" +
            "B-B\n" +
            "-A-"
        diamond(from = 'A', to = 'C') shouldEqual
            "--A--\n" +
            "-B-B-\n" +
            "C---C\n" +
            "-B-B-\n" +
            "--A--"
    }

    private fun diamond(from: Char, to: Char): String {
        return (from..to)
            .map {
                val padLeft = "-".repeat(to - it)
                val padRight = "-".repeat(it - from)
                padLeft + it + padRight
            }
            .map { it + it.reversed().drop(1) }
            .let { it + it.reversed().drop(1) }
            .joinToString("\n")
    }
}