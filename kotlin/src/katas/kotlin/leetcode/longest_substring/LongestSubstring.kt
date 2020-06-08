package katas.kotlin.leetcode.longest_substring

import datsok.shouldEqual
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
    val visited = HashSet<Char>()
    var from = 0
    var to = 0
    while (to < length) {
        val added = visited.add(this[to])
        if (!added) {
            while (this[from] != this[to]) {
                visited.remove(this[from])
                from++
            }
            from++
        }
        maxSize = maxOf(maxSize, to - from + 1)
        to++
    }
    return maxSize
}
