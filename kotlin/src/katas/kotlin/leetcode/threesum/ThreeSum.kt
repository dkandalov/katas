package katas.kotlin.leetcode.threesum

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/3sum/
 */
class ThreeSumTests {
    @Test fun `find all unique triplets in the array which gives the sum of zero`() {
        intArrayOf(-1, 0, 1).threeSum() shouldEqual listOf(listOf(-1, 0, 1))
        intArrayOf(-1, 0, 1, 2, -1, -4).threeSum() shouldEqual listOf(
            listOf(-1, 0, 1),
            listOf(-1, -1, 2)
        )
    }
}

private fun IntArray.threeSum(): List<List<Int>> {
    val result = ArrayList<List<Int>>()
    (0..size - 3).forEach { i ->
        (i + 1..size - 2).forEach { j ->
            (j + 1 .. size - 1).forEach { k ->
                if (this[i] + this[j] + this[k] == 0) {
                    result.add(listOf(this[i], this[j], this[k]))
                }
            }
        }
    }
    return result.map { it.sorted() }.distinct()
}
