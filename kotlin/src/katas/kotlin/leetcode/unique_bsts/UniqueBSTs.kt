package katas.kotlin.leetcode.unique_bsts

import kotlincommon.test.shouldEqual
import org.junit.Test

class UniqueBSTsTests {
    @Test fun `find how many structurally unique BST's that can store values from 1 to n`() {
        numberOfBSTs(1) shouldEqual 1
        numberOfBSTs(2) shouldEqual 2
        numberOfBSTs(3) shouldEqual 5
        numberOfBSTs(4) shouldEqual 14
    }

    private fun numberOfBSTs(n: Int): Int {
        return when (n) {
            1    -> 1
            2    -> 2
            else -> (0 until n).sumBy { i ->
                val less = maxOf(1, i)
                val greater = maxOf(1, n - i - 1)
                numberOfBSTs(less) * numberOfBSTs(greater)
            }
        }
    }
}