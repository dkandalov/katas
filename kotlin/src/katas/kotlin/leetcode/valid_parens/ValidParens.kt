package katas.kotlin.leetcode.valid_parens

import katas.kotlin.coroutines.Hello2.c
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
        "([)]".isValid() shouldEqual false
        "{[]}".isValid() shouldEqual true

        "())".isValid() shouldEqual false
        "(()".isValid() shouldEqual false
    }
}

private fun String.isValid(): Boolean {
    var s = ""
    val map = mapOf(
        '(' to ')',
        '[' to ']',
        '{' to '}'
    )
    (0 until length).forEach { i ->
        val c = this[i]
        if (map.keys.contains(c)) s += c
        else {
            val key = map.entries.find { it.value == c }!!.key
            if (s.lastOrNull() != key) return false
            s = s.dropLast(1)
        }
    }
    return s.isEmpty()
}
