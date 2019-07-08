package katas.kotlin.leetcode.longest_substring_palindrome

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestPalindrome {
    @Test fun `find longest palindromic substring`() {
        findLongestPalindrome("") shouldEqual ""
        findLongestPalindrome("a") shouldEqual "a"
        findLongestPalindrome("b") shouldEqual "b"
        findLongestPalindrome("c") shouldEqual "c"
    }

    @Test fun `check that string is a palindrome`() {
        "".isPalindrome() shouldEqual true
        "a".isPalindrome() shouldEqual true
        "ab".isPalindrome() shouldEqual false
        "aba".isPalindrome() shouldEqual true
        "abba".isPalindrome() shouldEqual true
    }

    private fun findLongestPalindrome(s: String): String {
        return s
    }

    private fun String.isPalindrome(): Boolean {
        if (length < 2) return true
        return if (first() == last()) substring(1, length - 2).isPalindrome() else false
    }
}
