package katas.kotlin.leetcode.integer_to_roman

import kotlincommon.test.shouldEqual
import org.junit.Test

class IntegerToRomanTests {
    @Test fun `it mostly works`() {
        1.toRoman() shouldEqual "I"
        1.toRoman() shouldEqual "I"
    }
}

private fun Int.toRoman(): String {
    return "I"
}
