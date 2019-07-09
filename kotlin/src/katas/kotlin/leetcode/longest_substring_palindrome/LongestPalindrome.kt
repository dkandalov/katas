package katas.kotlin.leetcode.longest_substring_palindrome

import kotlincommon.measureDuration
import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestPalindrome {
    @Test fun `trivial examples`() {
        findLongestPalindrome("") shouldEqual ""
        findLongestPalindrome("a") shouldEqual "a"
        findLongestPalindrome("b") shouldEqual "b"
        findLongestPalindrome("c") shouldEqual "c"
    }

    @Test fun `longest palindrome is at the start of the string`() {
        findLongestPalindrome("abc") shouldEqual "a"
        findLongestPalindrome("aabc") shouldEqual "aa"
        findLongestPalindrome("ababc") shouldEqual "aba"
    }

    @Test fun `longest palindrome is in the middle of the string`() {
        findLongestPalindrome("abbc") shouldEqual "bb"
        findLongestPalindrome("aabbbc") shouldEqual "bbb"
        findLongestPalindrome("abaabc") shouldEqual "baab"
    }

    @Test fun `longest palindrome is at the end of the string`() {
        findLongestPalindrome("abcc") shouldEqual "cc"
        findLongestPalindrome("abaab") shouldEqual "baab"
    }

    @Test fun `check that string is a palindrome`() {
        "".isPalindrome() shouldEqual true
        "a".isPalindrome() shouldEqual true
        "ab".isPalindrome() shouldEqual false
        "aba".isPalindrome() shouldEqual true
        "abba".isPalindrome() shouldEqual true
        "abcba".isPalindrome() shouldEqual true
        "abccba".isPalindrome() shouldEqual true
        "abcba_".isPalindrome() shouldEqual false
    }

    @Test fun `check that long string is a palindrome`() {
        val chars = (0..100_000).map { it.toChar() }
        val longPalindrome = (chars + chars.asReversed()).joinToString("")

        measureDuration {
            longPalindrome.isPalindrome() shouldEqual true
        }
    }

    private fun findLongestPalindrome_func(s: String): String {
        return (0..s.length)
            .flatMap { start -> (start..s.length).map { end -> start..end } }
            .map { range -> s.substring(range.first, range.last) }
            .filter { it.isPalindrome() }
            .fold("") { acc, it ->
                if (it.length > acc.length) it else acc
            }
    }

    private fun findLongestPalindrome(s: String): String {
        var result = ""
        (0..s.length).forEach { i ->
            (i..s.length).forEach { j ->
                val substring = s.substring(i, j)
                if (substring.isPalindrome() && substring.length > result.length) {
                    result = substring
                }
            }
        }
        return result
    }

    private tailrec fun String.isPalindrome(): Boolean {
        if (length < 2) return true
        return if (first() == last()) substring(1, length - 1).isPalindrome() else false
    }
}
