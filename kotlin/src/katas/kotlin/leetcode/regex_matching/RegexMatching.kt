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
        if (regex.isEmpty()) return s.isEmpty()
        when {
            regex.length >= 2 && regex[1] == '*' -> {
                return Matcher(s, regex.substring(2)).match() ||
                    ((s[0] == regex[0] || regex[0] == '.') && Matcher(s.substring(1), regex).match())
            }
            regex[0] == '.'                             -> {
                if (s.isEmpty()) return false
                return Matcher(s.substring(1), regex.substring(1)).match()
            }
            else                                        -> {
                if (s[0] != regex[0]) return false
                return Matcher(s.substring(1), regex.substring(1)).match()
            }
        }
    }
}

private fun String.matches(regex: String): Boolean {
    return Matcher(this, regex).match()
}
