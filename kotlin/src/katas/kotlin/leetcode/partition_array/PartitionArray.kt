package katas.kotlin.leetcode.partition_array

import kotlincommon.test.shouldEqual
import org.junit.Test

class PartitionArrayTests {
    @Test fun `partition an array such that all non-zero values are at the beginning`() {
        arrayOf(1).nonZeroFirst() shouldEqual arrayOf(1)
    }
}

private fun Array<Int>.nonZeroFirst(): Array<Int> {
    return this
}
