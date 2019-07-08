package katas.kotlin.leetcode.longest_substring_palindrome

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestPalindrome {
    @Test fun `trivial examples`() {
        findLongestPalindrome("") shouldEqual ""
        findLongestPalindrome("a") shouldEqual "a"
        findLongestPalindrome("b") shouldEqual "b"
        findLongestPalindrome("c") shouldEqual "c"
    }

    @Test fun `longest palindrom is at the start of the string`() {
        findLongestPalindrome("abc") shouldEqual "a"
        findLongestPalindrome("aabc") shouldEqual "aa"
        findLongestPalindrome("ababc") shouldEqual "aba"
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

    private fun findLongestPalindrome(s: String): String {
        return (0..s.length)
            .map { (0..it) }
            .map { s.substring(it.first, it.last) }
            .filter { it.isPalindrome() }
            .fold("") { acc, it ->
                if (it.length > acc.length) it else acc
            }
    }

    private tailrec fun String.isPalindrome(): Boolean {
        if (length < 2) return true
        return if (first() == last()) substring(1, length - 1).isPalindrome() else false
    }
}
