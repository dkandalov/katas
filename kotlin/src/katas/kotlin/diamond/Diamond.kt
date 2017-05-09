package katas.kotlin.diamond

import katas.kotlin.shouldEqual
import org.junit.Test

class DiamondTests {
    @Test fun `make diamonds of various sizes`() {
        makeDiamond(size = 1) shouldEqual "A"

        makeDiamond(size = 2) shouldEqual """
            |-A-
            |B-B
            |-A-
        """.trimMargin()

        makeDiamond(size = 3) shouldEqual """
            |--A--
            |-B-B-
            |C---C
            |-B-B-
            |--A--
        """.trimMargin()
    }

    @Test fun `make quarter diamonds of various sizes`() {
        makeQuarterDiamond(size = 1) shouldEqual "A"

        makeQuarterDiamond(size = 2) shouldEqual """
            |A-
            |-B
        """.trimMargin()

        makeQuarterDiamond(size = 3) shouldEqual """
            |A--
            |-B-
            |--C
        """.trimMargin()
    }

    @Test fun `reflect string`() {
        "A-\n-B".reflectLeft() shouldEqual "-A-\nB-B"
        "-A-\nB-B".reflectDown() shouldEqual "-A-\nB-B\n-A-"
    }

    private fun makeDiamond(size: Int): String {
        return makeQuarterDiamond(size).reflectLeft().reflectDown()
    }

    private fun String.reflectLeft() =
        split('\n').map{ it.reversed().dropLast(1) + it }.joinToString("\n")

    private fun String.reflectDown() =
        split('\n').let { it + it.reversed().drop(1) }.joinToString("\n")

    private fun makeQuarterDiamond(size: Int): String {
        val startLetter = 'A'
        return 0.until(size)
            .map { shift ->
                (startLetter + shift).toString()
                    .padStart(shift + 1, '-')
                    .padEnd(size, '-')
            }
            .joinToString("\n")
    }
}