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
                    return Matcher(s.substring(i), regex.substring(j + 2)).match() ||
                        ((s[i] == regex[j] || regex[j] == '.') && Matcher(s.substring(i + 1), regex.substring(j)).match())
                }
                regex[j] == '.'                             -> {
                    if (i >= s.length) return false
                    return Matcher(s.substring(i + 1), regex.substring(j + 1)).match()
                }
                else                                        -> {
                    if (s[i] != regex[j]) return false
                    return Matcher(s.substring(i + 1), regex.substring(j + 1)).match()
                }
            }
        }
        return i == s.length
    }
}

private fun String.matches(regex: String): Boolean {
    return Matcher(this, regex).match()
}
