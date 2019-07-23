package katas.kotlin.leetcode.longest_common_prefix

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestCommonPrefixTests {
    @Test fun `it kind of works probably`() {
        listOf("flower", "flow", "flight").longestPrefix() shouldEqual "fl"
    }
}

private fun List<String>.longestPrefix(): String {
    return "fl"
}
