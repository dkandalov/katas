package katas.kotlin.diamond

import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test

class Diamond7 {
    
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
        diamond(from = 'A', to = 'D') shouldEqual
            "---A---\n" +
            "--B-B--\n" +
            "-C---C-\n" +
            "D-----D\n" +
            "-C---C-\n" +
            "--B-B--\n" +
            "---A---"
    }

    private fun diamond(from: Char, to: Char): String {
        return (0..(to - from))
            .map {
                val padLeft = 0.until(to - from - it).map{ "-" }.join("")
                val padRight = 0.until(it).map{ "-" }.join("")
                padLeft + (from + it) + padRight
            }
            .map { it + it.reversed().drop(1) }
            .let { it + it.reversed().drop(1) }
            .join("\n")
    }
}