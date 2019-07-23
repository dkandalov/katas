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
        val j = 0
        when {
            regex.length >= 2 && regex[j + 1] == '*' -> {
                return Matcher(s.substring(0), regex.substring(2)).match() ||
                    ((s.first() == regex.first() || regex.first() == '.') && Matcher(s.substring(1), regex.substring(j)).match())
            }
            regex.first() == '.'                             -> {
                if (s.isEmpty()) return false
                return Matcher(s.substring(1), regex.substring(1)).match()
            }
            else                                        -> {
                if (s.first() != regex.first()) return false
                return Matcher(s.substring(1), regex.substring(1)).match()
            }
        }
    }
}

private fun String.matches(regex: String): Boolean {
    return Matcher(this, regex).match()
}
