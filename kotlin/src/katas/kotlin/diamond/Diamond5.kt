package katas.kotlin.diamond

import nonstdlib.join
import datsok.shouldEqual
import org.junit.Test

class Diamond5 {
    @Test fun `build a string diamond`() {
        diamond('A') shouldEqual "A"
        diamond('B') shouldEqual
            "-A-\n" +
            "B-B\n" +
            "-A-"
        diamond('C') shouldEqual
            "--A--\n" +
            "-B-B-\n" +
            "C---C\n" +
            "-B-B-\n" +
            "--A--"
    }
    
    @Test fun `build a string quarter-diamond`() {
        quarterDiamond(from = 'A', to = 'A') shouldEqual listOf("A")
        quarterDiamond(from = 'A', to = 'B') shouldEqual listOf(
            "-A",
            "B-"
        )
        quarterDiamond(from = 'A', to = 'C') shouldEqual listOf(
            "--A",
            "-B-",
            "C--"
        )
    }

    private fun diamond(to: Char): String {
        return quarterDiamond(to = to)
            .mirrorRight()
            .mirrorDown()
            .joinToString("\n")
    }

    private fun quarterDiamond(from: Char = 'A', to: Char): List<String> {
        return (from..to)
            .map{
                (0..(to - it - 1)).map{ '-' }.join("") +
                it.toString() +
                (0..(it - from - 1)).map{ '-' }.join("")
            }
    }

    private fun List<String>.mirrorRight() = this.map { it + it.reversed().drop(1) }

    private fun List<String>.mirrorDown() = this + this.reversed().drop(1)
}
