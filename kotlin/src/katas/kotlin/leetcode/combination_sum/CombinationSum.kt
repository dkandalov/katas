package katas.kotlin.leetcode.combination_sum

import kotlincommon.test.shouldEqual
import org.junit.Test

class CombinationSumTests {
    @Test fun `find all unique combinations where numbers sum up to target`() {
        emptyList<Int>().combinations(target = 1) shouldEqual emptyList()

        listOf(1).combinations(target = 0) shouldEqual listOf(emptyList())
        listOf(1).combinations(target = 1) shouldEqual listOf(listOf(1))
//        listOf(1).combinations(target = 2) shouldEqual listOf(listOf(1, 1))
//        listOf(1, 2).combinations(target = 2) shouldEqual listOf(listOf(1, 1), listOf(2))
//
//        listOf(2, 3, 6, 7).combinations(target = 7) shouldEqual listOf(listOf(2, 2, 3), listOf(7))
//        listOf(2, 3, 5).combinations(target = 8) shouldEqual listOf(listOf(2, 2, 2, 2), listOf(2, 3, 3), listOf(3, 5))
//
//        listOf(100).combinations(target = 1) shouldEqual emptyList()
    }
}

private fun List<Int>.combinations(target: Int): List<List<Int>> {
    if (target == 0) return listOf(emptyList())
    if (isEmpty()) return emptyList()
    return combinations(target - first()).map { it + first() } +
           subList(1, size).combinations(target)
}
