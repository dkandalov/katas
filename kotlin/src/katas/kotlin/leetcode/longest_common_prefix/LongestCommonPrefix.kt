package katas.kotlin.leetcode.longest_common_prefix

import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test

class LongestCommonPrefixTests {
    @Test fun `it kind of works probably`() {
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
    strings.first().substring(0, prefixLength).printed()

    val commonLength = strings.map { it.length }.min()!!
    val i = commonLength.downTo(0).first { i ->
        strings.map { it.substring(0, i) }.windowed(2).all { it[0] == it[1] }
    }
    return strings.first().substring(0, i)
}
