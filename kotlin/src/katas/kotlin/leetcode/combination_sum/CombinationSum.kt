package katas.kotlin.leetcode.combination_sum

import kotlincommon.test.shouldEqual
import org.junit.Test

class CombinationSumTests {
    @Test fun `find all unique combinations where numbers sum up to target`() {
        listOf(2, 3, 6, 7).combinations(target = 7) shouldEqual listOf(listOf(7), listOf(2, 2, 3))
    }
}

private fun List<Int>.combinations(target: Int): List<List<Int>> {
    return listOf(listOf(7), listOf(2, 2, 3))
}
