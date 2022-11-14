package katas.kotlin.leetcode.valid_number

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// https://leetcode.com/problems/valid-number ✅
//
// A valid number can be split up into these components (in order):
//    A decimal number or an integer.
//    (Optional) An 'e' or 'E', followed by an integer.
//
// A decimal number can be split up into these components (in order):
//    (Optional) A sign character (either '+' or '-').
//    One of the following formats:
//        One or more digits, followed by a dot '.'.
//        One or more digits, followed by a dot '.', followed by one or more digits.
//        A dot '.', followed by one or more digits.
//
// An integer can be split up into these components (in order):
//    (Optional) A sign character (either '+' or '-').
//    One or more digits.
//
// For example, all the following are valid numbers: ["2", "0089", "-0.1", "+3.14", "4.", "-.9", "2e10", "-90E3", "3e+7", "+6e-1", "53.5e93", "-123.456e789"],
// while the following are not valid numbers: ["abc", "1a", "1e", "e3", "99e2.5", "--6", "-+3", "95a54e53"].
//
// Given a string s, return true if s is a valid number.
//
// Constraints:
//    1 <= s.length <= 20
//    s consists of only English letters (both uppercase and lowercase), digits (0-9), plus '+', minus '-', or dot '.'.
//
class ValidNumber {
    @Test fun `valid numbers`() {
        listOf("2", "0089", "-0.1", "+3.14", "4.", "-.9", "2e10", "-90E3", "3e+7", "+6e-1", "53.5e93", "-123.456e789")
            .filterNot { isNumber(it) } shouldEqual emptyList()
    }

    @Test fun `invalid numbers`() {
        listOf("abc", "1a", "1e", "e3", "99e2.5", "--6", "-+3", "95a54e53")
            .filter { isNumber(it) } shouldEqual emptyList()
    }

    @Test fun `is integer`() {
        listOf("1", "123", "-123", "+123").filterNot { isNumber(it) } shouldEqual emptyList()
        listOf("", "+", "123-", "1-23", "+-123").filter { isNumber(it) } shouldEqual emptyList()
    }

    @Test fun `is decimal`() {
        listOf("1.", "123.", "1.23", "+1.23", ".123", "+.123").filterNot { isNumber(it) } shouldEqual emptyList()
        listOf("+.", ".", "1.2.3").filter { isNumber(it) } shouldEqual emptyList()
    }

    private fun isNumber(s: String): Boolean {
        val int = """[-+]?\d+"""
        val decimal = """[-+]?(?:\d+\.\d*|\.\d+)"""

        if (Regex(int).matches(s)) return true
        if (Regex(decimal).matches(s)) return true
        if (Regex("""(?x)
            |(?:$int|$decimal)
            |(?:[eE]$int)?
            |""".trimMargin()).matches(s)) return true
        return false
    }
}

fun isNumber_old(s: String): Boolean {
    var i = 0
    fun letter(): Char =
        if (i < s.length) s[i] else 0.toChar()
    fun digits(): Boolean {
        val match = letter() in '0'..'9'
        if (!match) return false
        i++
        digits()
        return true
    }
    fun exponent(): Boolean {
        if (letter() != 'e') return false
        i++
        if (letter() == '-' || letter() == '+') i++
        if (!digits()) return false
        if (letter() == '.') {
            i++
            digits()
        }
        return true
    }
    fun factor(): Boolean {
        if (letter() == '.') {
            i++
            return digits()
        } else {
            return exponent()
        }
    }
    fun spaces(): Boolean {
        if (letter() == ' ') {
            i++
            spaces()
        }
        return true
    }
    fun number(): Boolean {
        spaces()
        if (letter() == '-' || letter() == '+') i++
        if (!digits()) return false
        factor()
        spaces()
        return true
    }
    return number() && i == s.length
}