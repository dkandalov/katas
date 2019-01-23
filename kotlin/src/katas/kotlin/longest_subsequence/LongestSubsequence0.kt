package katas.kotlin.longest_subsequence

import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.random.Random

class LongestSubsequence0 {
    @Test fun `longest subsequence of two strings`() {
        longestSubsequence("", "") shouldEqual ""
        longestSubsequence("a", "") shouldEqual ""
        longestSubsequence("", "a") shouldEqual ""

        longestSubsequence("a", "a") shouldEqual "a"
        longestSubsequence("abc", "abc") shouldEqual "abc"

        longestSubsequence("abc", "_abc") shouldEqual "abc"
        longestSubsequence("abc", "__abc") shouldEqual "abc"
        longestSubsequence("abc", "abc_") shouldEqual "abc"
        longestSubsequence("abc", "abc__") shouldEqual "abc"
        longestSubsequence("abc", "_abc_") shouldEqual "abc"

        longestSubsequence("abc", "a_bc") shouldEqual "abc"
        longestSubsequence("abc", "ab_c") shouldEqual "abc"
        longestSubsequence("abc", "a_b_c") shouldEqual "abc"
        longestSubsequence("abc", "_a_b_c") shouldEqual "abc"
        longestSubsequence("abc", "_a_b_c_") shouldEqual "abc"
        longestSubsequence("_a_b_c_", "abc") shouldEqual "abc"

        longestSubsequence("_a_b_c_", "-a-b-c-") shouldEqual "abc"
        longestSubsequence("_a_a_a_", "-a-a-a-") shouldEqual "aaa"

        longestSubsequence("abcde", "ab__cd__cde") shouldEqual "abcde"

        val s1 = Random.nextInt().toString().padStart(20, '_').printed()
        val s2 = Random.nextInt().toString().padStart(20, '_').printed()
        val mixed = s1.zip(s2).joinToString("") { it.first.toString() + it.second}.printed()
        longestSubsequence(s1, mixed) shouldEqual s1
        longestSubsequence(s2, mixed) shouldEqual s2
    }

    private fun longestSubsequence(s1: String, s2: String): String {
        if (s1.isEmpty() || s2.isEmpty()) return ""
        val c = s1.first()
        val i = s2.indexOf(c)
        return if (i == -1) longestSubsequence(s1.drop(1), s2)
        else c.toString() + longestSubsequence(s1.drop(1), s2.drop(i + 1))
    }
}