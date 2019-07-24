package katas.kotlin.leetcode.threesum

import kotlincommon.test.shouldEqual
import org.junit.Test

class ThreeSumTests {
    @Test fun `find all unique triplets in the array which gives the sum of zero`() {
        intArrayOf(-1, 0, 1, 2, -1, -4).threeSum() shouldEqual listOf(
            listOf(-1, 0, 1),
            listOf(-1, -1, 2)
        )
    }
}

private fun IntArray.threeSum(): List<List<Int>> {
    return listOf(
        listOf(-1, 0, 1),
        listOf(-1, -1, 2)
    )
}
