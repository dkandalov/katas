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
    val map = mapOf(
        1000 to "M",
        500 to "D",
        100 to "C",
        50 to "L",
        10 to "X",
        5 to "V",
        1 to "I"
    )
    map.entries.find { this >= it.key } ?: return ""
    return map[this] ?: ""
}
