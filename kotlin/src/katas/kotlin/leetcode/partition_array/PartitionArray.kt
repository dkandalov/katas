package katas.kotlin.leetcode.partition_array

import kotlincommon.swap
import kotlincommon.test.shouldEqual
import org.junit.Test

class PartitionArrayTests {
    @Test fun `partition an array such that all non-zero values are at the beginning`() {
        arrayOf(0).nonZeroFirst() shouldEqual arrayOf(0)
        arrayOf(1).nonZeroFirst() shouldEqual arrayOf(1)

        arrayOf(0, 0).nonZeroFirst() shouldEqual arrayOf(0, 0)
        arrayOf(1, 0).nonZeroFirst() shouldEqual arrayOf(1, 0)
        arrayOf(0, 1).nonZeroFirst() shouldEqual arrayOf(1, 0)
        arrayOf(1, 1).nonZeroFirst() shouldEqual arrayOf(1, 1)

        arrayOf(1, 2, 0).nonZeroFirst() shouldEqual arrayOf(1, 2, 0)
        arrayOf(1, 0, 2).nonZeroFirst() shouldEqual arrayOf(1, 2, 0)
        arrayOf(0, 1, 2).nonZeroFirst() shouldEqual arrayOf(2, 1, 0)

        arrayOf(5, 0, 1, 2, 0, -4).nonZeroFirst() shouldEqual arrayOf(5, -4, 1, 2, 0, 0)
    }
}

private fun Array<Int>.nonZeroFirst(): Array<Int> {
    var i = 0
    var j = size - 1
    while (i < j) {
        if (this[i] != 0) continue

        while (i < j && this[j] == 0) j--
        if (i >= j) return this
        swap(i, j)
        i++
    }
    return this
}
