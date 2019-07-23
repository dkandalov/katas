package katas.kotlin.leetcode.longest_common_prefix

import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestCommonPrefixTests {
    @Test fun `it kind of works probably`() {
        listOf("flower", "flow", "flight").longestPrefix() shouldEqual "fl"
        listOf("dog", "racecar", "car").longestPrefix() shouldEqual ""
    }
}

private fun List<String>.longestPrefix(): String {
    val commonLength = map { it.length }.min()!!
    val i = commonLength.downTo(0).first { i ->
        map { it.substring(0, i) }.windowed(2).all { it[0] == it[1] }
    }
    return first().substring(0, i)
}
