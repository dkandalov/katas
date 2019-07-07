package katas.kotlin.leetcode.longestsubstring

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/longest-substring-without-repeating-characters/
 */
class LongestSubstring {
    @Test fun `find the length of the longest substring without repeating characters`() {
        "".longestSubstring() shouldEqual 0
        "a".longestSubstring() shouldEqual 1
        "abc".longestSubstring() shouldEqual 3
        "abca".longestSubstring() shouldEqual 3
        "aabbc".longestSubstring() shouldEqual 2

        "abcabcbb".longestSubstring() shouldEqual 3
        "bbbbb".longestSubstring() shouldEqual 1
        "pwwkew".longestSubstring() shouldEqual 3
    }
}

private fun String.longestSubstring(): Int {
    var maxSize = 0
    val chars = HashSet<Char>()
    var i = 0
    var j = 0
    while (j < length) {
        val added = chars.add(this[j])
        if (!added) {
            while (this[i] != this[j]) {
                chars.remove(this[i])
                i++
            }
            i++
        }
        maxSize = maxOf(maxSize, j - i + 1)
        j++
    }
    return maxSize
}
