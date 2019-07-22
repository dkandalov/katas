package katas.kotlin.leetcode.regex_matching

import kotlincommon.test.shouldEqual
import org.junit.Test

class RegexMatchingTests {
    @Test fun `it mostly works`() {
        "".matches("") shouldEqual true
        "a".matches("a") shouldEqual true
        "a".matches("b") shouldEqual false

        "a".matches(".") shouldEqual true
        "ab".matches("a.") shouldEqual true
        "ab".matches(".b") shouldEqual true
        "ab".matches("..") shouldEqual true
        "ab".matches("X.") shouldEqual false
        "ab".matches("...") shouldEqual false

        "ab".matches("*") shouldEqual true
        "ab".matches("**") shouldEqual true
        "ab".matches("***") shouldEqual true
        "abc".matches("a*") shouldEqual true
        "abc".matches("ab*") shouldEqual true
        "abc".matches("abc*") shouldEqual true
        "abc".matches("*X") shouldEqual false
    }
}

private fun String.matches(regex: String): Boolean {
    (0 until regex.length).forEach { i ->
        val char = regex[i]
        if (char == '*') {
            return (i .. length).any { j ->
                substring(j).matches(regex.substring(i + 1))
            }
        }
        if (i >= length) return false
        if (char != '.' && char != this[i]) return false
    }
    return true
}
