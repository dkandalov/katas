package katas.kotlin.leetcode.valid_parens

import kotlincommon.test.shouldEqual
import org.junit.Test

class ValidParensTests {
    @Test fun `determine if the input string is valid`() {
        "".isValid() shouldEqual true
    }
}

private fun String.isValid(): Boolean {
    return true
}
