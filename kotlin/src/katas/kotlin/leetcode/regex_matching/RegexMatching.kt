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

        "ab".matches("a*b") shouldEqual true
//        "ab".matches("a*") shouldEqual false
//        "ab".matches("**") shouldEqual true
//        "ab".matches("***") shouldEqual true
//
//        "abc".matches("a*") shouldEqual true
//        "abc".matches("ab*") shouldEqual true
//        "abc".matches("abc*") shouldEqual true
//        "abc".matches("*c") shouldEqual true
//        "abc".matches("*bc") shouldEqual true
//        "abc".matches("*abc") shouldEqual true
//
//        "abc".matches("*X") shouldEqual false
    }
}

private class Matcher(val s: String, val regex: String) {
    fun match(): Boolean {
        var i = 0
        var j = 0
        while (j < regex.length) {
            when {
                regex[j] == '.' -> {
                    if (i >= s.length) return false
                    i++
                    j++
                }
                j < regex.length -1 && regex[j + 1] == '*' -> {
                    val char = regex[j]
                    return (i until s.length)
                        .takeWhile { s[it] == char }
                        .map { Matcher(s.substring(it + 1), regex.substring(j + 2)) }
                        .any { it.match() }
                }
                else            -> {
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
