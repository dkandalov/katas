package katas.kotlin.diamond

import kotlincommon.test.shouldEqual
import org.junit.Test

class Diamond9 {

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
        if (from == to) return from.toString()

        fun Char.prefix() = (0.until(to - this@prefix)).toList().joinToString(""){ "-" }
        fun Char.postfix() = (0.until(this@postfix - from)).toList().joinToString(""){ "-" }
        fun String.reflectRight() = this + this.reversed().drop(1)
        fun <E> List<E>.reflectDown() = this + this.reversed().drop(1)

        return (from..to)
            .toList()
            .map { it.prefix() + it + it.postfix() }
            .map { it.reflectRight() }
            .reflectDown()
            .joinToString("\n")
    }
}


