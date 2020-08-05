package katas.kotlin.leetcode.atoi

import datsok.shouldEqual
import org.junit.Test

// "" -> 0
// "42" -> 42
// "   -42" -> 42
// "4193 with words" -> 4193
// "words and 987" -> 0
// "-91283472332" -> -2147483648
//
// https://leetcode.com/problems/string-to-integer-atoi ✅
fun atoi(s: String): Int {
    if (s.isEmpty()) return 0
    var i = 0

    fun skipSpaces() {
        while (i < s.length && s[i] == ' ') i++
    }

    fun skipZeros() {
        while (i < s.length && s[i] == '0') i++
    }

    fun readOptionalSign(): Long {
        if (i == s.length) return 1
        if (s[i] == '-') {
            i++
            return -1
        }
        if (s[i] == '+') i++
        return 1
    }

    fun readDigits(): String {
        var result = ""
        while (i < s.length && s[i].isDigit()) {
            result += s[i]
            i++
        }
        return result
    }

    fun toInt(sign: Long, digits: String): Int {
        var result = 0
        var mult = 1L
        digits.reversed().forEach { digit ->
            val newResult: Long = result + sign * mult * (digit - '0')
            result = newResult.toInt()
            mult *= 10
            if (result.toLong() != newResult || mult < 0) {
                return if (sign > 0) Int.MAX_VALUE else Int.MIN_VALUE
            }
        }
        return result
    }
    skipSpaces()
    val sign = readOptionalSign()
    skipZeros()
    val digits = readDigits()
    return toInt(sign, digits)
}

class AtoiTests {
    @Test fun `it works`() {
        atoi("") shouldEqual 0
        atoi("  ") shouldEqual 0
        atoi("42") shouldEqual 42
        atoi("+42") shouldEqual 42
        atoi("-42") shouldEqual -42
        atoi("   -42") shouldEqual -42
        atoi("4193 with words") shouldEqual 4193
        atoi("words and 987") shouldEqual 0

        atoi("  0000000000012345678") shouldEqual 12345678
        atoi("-000000000000001") shouldEqual -1

        atoi("2147483646") shouldEqual 2147483646
        atoi("2147483647") shouldEqual 2147483647
        atoi("2147483648") shouldEqual 2147483647
        atoi("91283472332") shouldEqual 2147483647

        atoi("-2147483647") shouldEqual -2147483647
        atoi("-2147483648") shouldEqual -2147483648
        atoi("-2147483649") shouldEqual -2147483648
        atoi("-91283472332") shouldEqual -2147483648

        atoi("10000000000000000000000000000000000000000000000000000000" +
                 "000000000000000000000000000000000000000000000000522545459") shouldEqual 2147483647
        atoi("    10522545459") shouldEqual 2147483647
    }
}