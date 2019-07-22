package katas.kotlin.leetcode.integer_to_roman

import kotlincommon.test.shouldEqual
import org.junit.Test

class IntegerToRomanTests {
    @Test fun `it mostly works`() {
        1.toRoman() shouldEqual "I"
        5.toRoman() shouldEqual "V"
        10.toRoman() shouldEqual "X"
        50.toRoman() shouldEqual "L"
        100.toRoman() shouldEqual "C"
        500.toRoman() shouldEqual "D"
        1000.toRoman() shouldEqual "M"


    }
}

private fun Int.toRoman(): String {
    return when (this) {
        1000 -> "M"
        500  -> "D"
        100  -> "C"
        50   -> "L"
        10   -> "X"
        5    -> "V"
        1    -> "I"
        else -> ""
    }
}