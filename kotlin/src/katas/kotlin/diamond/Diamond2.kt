package katas.kotlin.diamond

import kotlincommon.test.shouldEqual
import org.junit.Test

class Diamonds2Tests {
    @Test fun `create diamonds of various size`() {
        diamond(size = 1) shouldEqual """
                |A
            """.trimMargin()

        diamond(size = 2) shouldEqual """
                |-A-
                |B-B
                |-A-
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

    @Test fun `create quarter diamonds of various size`() {
        quarterDiamond(size = 1) shouldEqual """
                |A
            """.trimMargin()

        quarterDiamond(size = 2) shouldEqual """
                |-A
                |B-
            """.trimMargin()

        quarterDiamond(size = 3) shouldEqual """
                |--A
                |-B-
                |C--
            """.trimMargin()
    }

    @Test fun `reflect string`() {
        "-A\nB-".reflectRight() shouldEqual "-A-\nB-B"
        "-A-\nB-B".reflectDown() shouldEqual "-A-\nB-B\n-A-"
    }

    private fun quarterDiamond(size: Int, startLetter: Char = 'A'): String {
        return 0.until(size)
            .map{ shift ->
                (startLetter + shift).toString()
                    .padStart(size - shift, '-')
                    .padEnd(size, '-')
            }
            .joinToString("\n")
    }

    private fun diamond(size: Int) =
        quarterDiamond(size).reflectRight().reflectDown()

    private fun String.reflectDown() =
        this.split("\n")
            .let { it + it.reversed().drop(1) }
            .joinToString("\n")

    private fun String.reflectRight() =
        this.split("\n")
            .map{ it + it.reversed().drop(1) }
            .joinToString("\n")
}
