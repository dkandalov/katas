package katas.kotlin.leetcode.longestsubstring

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestSubstring {
    @Test fun `find the length of the longest substring without repeating characters`() {
        "".longestSubstring() shouldEqual 0
        "a".longestSubstring() shouldEqual 1
        "abc".longestSubstring() shouldEqual 3
        "abca".longestSubstring() shouldEqual 3
    }
}

private fun String.longestSubstring(): Int {
    var maxSize = 0
    val chars = HashSet<Char>()
    0.until(length).forEach { i ->
        var j = i
        while (j < length && chars.add(this[j])) j++
        maxSize = maxOf(maxSize, chars.size)
        chars.clear()
    }
    return maxSize
}
