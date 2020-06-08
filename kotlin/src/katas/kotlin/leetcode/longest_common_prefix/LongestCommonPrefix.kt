package katas.kotlin.leetcode.longest_common_prefix

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/longest-common-prefix
 */
class LongestCommonPrefixTests {
    @Test fun `find the longest common prefix string amongst an array of strings`() {
        longestCommonPrefix(arrayOf("flower")) shouldEqual "flower"
        longestCommonPrefix(arrayOf("flower", "flow")) shouldEqual "flow"
        longestCommonPrefix(arrayOf("flower", "flow", "flight")) shouldEqual "fl"
        longestCommonPrefix(arrayOf("dog", "racecar", "car")) shouldEqual ""
    }
}

private fun longestCommonPrefix(strings: Array<String>): String {
    var prefixLength = strings.first().length
    var index = prefixLength - 1
    while (index >= 0) {
        strings.forEach { s ->
            val match = index < s.length && s[index] == strings.first()[index]
            if (!match) prefixLength = index
        }
        index--
    }
    return strings.first().substring(0, prefixLength)
}
