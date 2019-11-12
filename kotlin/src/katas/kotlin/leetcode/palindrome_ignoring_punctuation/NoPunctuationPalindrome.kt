package katas.kotlin.leetcode.palindrome_ignoring_punctuation

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * Given a string with punctuations, upper case and lower case letters,
 * check if it can be read the same from the start and from the end by ignoring punctuations and capitalization.
 */
class NoPunctuationPalindromeTests {
    private val isPalindrome: (String) -> Boolean = ::isPalindrome

    @Test fun examples() {
        isPalindrome("") shouldEqual true
        isPalindrome(",,,") shouldEqual true
        isPalindrome(",,,a") shouldEqual true
        isPalindrome("aba") shouldEqual true
        isPalindrome("abba") shouldEqual true
        isPalindrome("abcba") shouldEqual true
        isPalindrome("a,bA") shouldEqual true
        isPalindrome("a,b,c") shouldEqual false
        isPalindrome(",,,abba,") shouldEqual true
        isPalindrome(",,,abbx,") shouldEqual false
        isPalindrome("--ZZX") shouldEqual false
    }
}

private fun isPalindrome(s: String): Boolean {
    if (s.isEmpty()) return true
    var i = 0
    var j = s.length - 1
    while (i <= j) {
        while (i <= j && !s[i].isLetter()) i++
        while (i <= j && !s[j].isLetter()) j--
        if (i > j) break
        if (s[i++].toLowerCase() != s[j--].toLowerCase()) return false
    }
    return true
}

private fun isPalindrome_(s: String): Boolean {
    val s2 = s.filter { it.isLetter() }.toLowerCase()
    val midIndex = s2.length / 2
    return s2.substring(0, midIndex) ==
        s2.substring(if (s2.length % 2 == 0) midIndex else midIndex + 1).reversed()
}
