package katas.kotlin.leetcode.integer_to_roman

import kotlincommon.test.shouldEqual
import org.junit.Test

class IntegerToRomanTests {
    @Test fun `it mostly works`() {
        1.toRoman() shouldEqual "I"
        2.toRoman() shouldEqual "II"
        3.toRoman() shouldEqual "III"
        4.toRoman() shouldEqual "IV"
        5.toRoman() shouldEqual "V"
        6.toRoman() shouldEqual "VI"
        7.toRoman() shouldEqual "VII"
        8.toRoman() shouldEqual "VIII"
        9.toRoman() shouldEqual "IX"
        10.toRoman() shouldEqual "X"
        11.toRoman() shouldEqual "XI"
        12.toRoman() shouldEqual "XII"
        40.toRoman() shouldEqual "XL"
        50.toRoman() shouldEqual "L"
        58.toRoman() shouldEqual "LVIII"
        59.toRoman() shouldEqual "LIX"
        89.toRoman() shouldEqual "LXXXIX"
        90.toRoman() shouldEqual "XC"
        100.toRoman() shouldEqual "C"
        400.toRoman() shouldEqual "CD"
        500.toRoman() shouldEqual "D"
        900.toRoman() shouldEqual "CM"
        1000.toRoman() shouldEqual "M"
        1994.toRoman() shouldEqual "MCMXCIV"
        100_000.toRoman() shouldEqual "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"
    }
}

private fun Int.toRoman(): String {
    val map = mapOf(
        1000 to "M", 900 to "CM",
        500 to "D", 400 to "CD",
        100 to "C", 90 to "XC",
        50 to "L", 40 to "XL",
        10 to "X", 9 to "IX",
        5 to "V", 4 to "IV",
        1 to "I"
    )
    var result = ""
    var n = this
    while (n != 0) {
        val (number, numeral) = map.entries.find { n >= it.key }!!
        result += numeral
        n -= number
    }
    return result
}
