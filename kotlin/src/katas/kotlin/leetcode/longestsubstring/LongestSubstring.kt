package katas.kotlin.leetcode.longestsubstring

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestSubstring {
    @Test fun `find the length of the longest substring without repeating characters`() {
        "".longestSubstring() shouldEqual 0
    }
}

private fun String.longestSubstring(): Int {
    return 0
}
