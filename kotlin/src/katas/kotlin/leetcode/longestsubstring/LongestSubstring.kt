package katas.kotlin.leetcode.longestsubstring

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestSubstring {
    @Test fun `find the length of the longest substring without repeating characters`() {
        "".longestSubstring() shouldEqual 0
        "abc".longestSubstring() shouldEqual 3
        "abca".longestSubstring() shouldEqual 3
    }
}

private fun String.longestSubstring(): Int {
    return toSet().size
}
