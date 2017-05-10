package katas.kotlin.`sn-dojo`

import katas.kotlin.join
import katas.kotlin.shouldEqual
import org.junit.Test

class DiamondTests {
    @Test fun `number of dashes`() {
        numberOfDashes(letter = 'A') shouldEqual 0
        numberOfDashes(letter = 'B') shouldEqual 1
        numberOfDashes(letter = 'C') shouldEqual 3
        numberOfDashes(letter = 'D') shouldEqual 5
    }

    @Test fun `generate triangle`() {
        triangle(letter = 'C') shouldEqual """
            |A
            |B-B
            |C---C
        """.trimMargin()
    }

    private fun triangle(letter: Char): String {
        return 'A'.rangeTo(letter).map { letter ->
            val dashes = '-'.repeat(numberOfDashes(letter))
            val result = letter + dashes
            return if (dashes != "") result + letter else result
        }.joinToString("\n")
    }

    private fun numberOfDashes(letter: Char): Int {
        val distance = letter - 'A'
        return if (distance == 0) distance else 2 * distance - 1
    }

    private fun drawLetters(letter: Char): String {
        return 'A'.rangeTo(letter).joinToString("")
    }
}

private fun Char.repeat(n: Int) = 0.until(n).map{this}.join("")
