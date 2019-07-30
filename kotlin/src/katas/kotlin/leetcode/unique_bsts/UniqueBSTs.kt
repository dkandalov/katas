package katas.kotlin.leetcode.unique_bsts

import kotlincommon.test.shouldEqual
import org.junit.Test

class UniqueBSTsTests {
    @Test fun `find how many structurally unique BST's that can store values from 1 to n`() {
        numberOfBSTs(1) shouldEqual 1
    }

    private fun numberOfBSTs(n: Int): Int {
        return 1
    }
}