package katas.kotlin.leetcode

import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test
import java.util.*

class SpaceEscapeTests {
    @Test fun `it works fine`() {
        "".spaceEscape().printed() shouldEqual ""
        "abc".spaceEscape().printed() shouldEqual "abc"
        "   ".spaceEscape().printed() shouldEqual "%20"
        "      ".spaceEscape().printed() shouldEqual "%20%20"

        " a  ".spaceEscape().printed() shouldEqual "%20a"
        "a   ".spaceEscape().printed() shouldEqual "a%20"
        " abc  ".spaceEscape().printed() shouldEqual "%20abc"
        "a bc  ".spaceEscape().printed() shouldEqual "a%20bc"
        "ab c  ".spaceEscape().printed() shouldEqual "ab%20c"
        "abc   ".spaceEscape().printed() shouldEqual "abc%20"

        "a      ".spaceEscape().printed() shouldEqual "a%20%20"
        " a     ".spaceEscape().printed() shouldEqual "%20a%20"
        "  a    ".spaceEscape().printed() shouldEqual "%20%20a"
    }
}

private fun String.spaceEscape() = toCharArray().spaceEscape().joinToString("")

private fun CharArray.spaceEscape(): CharArray {
    val stack = LinkedList<Char>()
    var charCount = 0
    var actualSize = size
    var i = 0
    while (charCount < actualSize) {
        if (stack.isNotEmpty()) {
            stack.push(this[i])
            this[i] = stack.removeLast()
        }
        if (this[i] == ' ') {
            actualSize -= 2
            if (i + 1 < actualSize) stack.addFirst(this[i + 1])
            if (i + 2 < actualSize) stack.addFirst(this[i + 2])
            this[i] = '%'
            this[i + 1] = '2'
            this[i + 2] = '0'
            i += 2
        }
        i++
        charCount++
    }
    return this
}
