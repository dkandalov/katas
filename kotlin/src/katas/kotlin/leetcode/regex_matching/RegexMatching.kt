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

        "aa".matches("a*") shouldEqual true
        "ab".matches("a*b") shouldEqual true
        "ab".matches("a*b*") shouldEqual true
        "ab".matches("a*b*c*") shouldEqual true
        "ab".matches("a*") shouldEqual false
        "ab".matches("X*ab") shouldEqual true

        "aa".matches("a") shouldEqual false
        "aa".matches("a*") shouldEqual true
        "ab".matches(".*") shouldEqual true
        "aab".matches("c*a*b") shouldEqual true
        "mississippi".matches("mis*is*p*.") shouldEqual false
    }
}

private class Matcher(val s: String, val regex: String) {
    fun match(): Boolean {
        var i = 0
        var j = 0
        while (j < regex.length) {
            when {
                j < regex.length - 1 && regex[j + 1] == '*' -> {
                    val char = regex[j]
                    val mismatchIndex = if (char == '.') s.length else (i until s.length).find { s[it] != char } ?: s.length
                    return (i..mismatchIndex)
                        .map { Matcher(s.substring(it), regex.substring(j + 2)) }
                        .any { it.match() }
                }
                regex[j] == '.'                             -> {
                    if (i >= s.length) return false
                    i++
                    j++
                }
                else                                        -> {
                    if (s[i] != regex[j]) return false
                    i++
                    j++
                }
            }
        }
        return i == s.length
    }
}

private fun String.matches(regex: String): Boolean {
    return Matcher(this, regex).match()
}
