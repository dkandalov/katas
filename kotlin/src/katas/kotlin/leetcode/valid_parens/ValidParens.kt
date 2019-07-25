package katas.kotlin.leetcode.valid_parens

import kotlincommon.test.shouldEqual
import org.junit.Test

class ValidParensTests {
    @Test fun `determine if the input string is valid`() {
        "".isValid() shouldEqual true
        "()".isValid() shouldEqual true
        "[]".isValid() shouldEqual true
        "{}".isValid() shouldEqual true
        "()[]{}".isValid() shouldEqual true
        "(]".isValid() shouldEqual false
    }
}

private fun String.isValid(): Boolean {
    var s = ""
    (0 until length).forEach { i ->
        when (val c = this[i]) {
            '(', '[', '{' -> s += c
            ')'           -> {
                if (s.last() != '(') return false
                s = s.dropLast(1)
            }
            ']'           -> {
                if (s.last() != '[') return false
                s = s.dropLast(1)
            }
            '}'           -> {
                if (s.last() != '{') return false
                s = s.dropLast(1)
            }
        }
    }
    return true
}
