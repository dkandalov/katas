package katas.kotlin.leetcode.unique_bsts

import kotlincommon.test.shouldEqual
import org.junit.Test

class UniqueBSTsTests {
    @Test fun `find how many structurally unique BST's that can store values from 1 to n`() {
        numberOfBSTs(1) shouldEqual 1
        numberOfBSTs(2) shouldEqual 2
        numberOfBSTs(3) shouldEqual 5
    }

    private fun numberOfBSTs(n: Int): Int {
        return if (n == 1) 1
        else if (n == 2) 2
        else numberOfBSTs(n - 1) * 2 + 1
    }
}