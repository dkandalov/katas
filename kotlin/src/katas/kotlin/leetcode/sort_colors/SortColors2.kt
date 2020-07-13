package katas.kotlin.leetcode.sort_colors

import datsok.shouldEqual
import nonstdlib.permutationsSequence
import nonstdlib.printed
import org.junit.jupiter.api.Test

class SortColors2 {
    @Test fun `it works`() {
        checkPermutationsOf(arrayOf(0))
        checkPermutationsOf(arrayOf(0, 1))
        checkPermutationsOf(arrayOf(0, 1, 2))
        checkPermutationsOf(arrayOf(0, 0, 1, 2))
        checkPermutationsOf(arrayOf(0, 1, 1, 2))
        checkPermutationsOf(arrayOf(0, 1, 2, 2))
        checkPermutationsOf(arrayOf(0, 0, 1, 1, 2, 2))
    }

    private fun checkPermutationsOf(ints: Array<Int>) {
        ints.toList().permutationsSequence().map { it.toTypedArray() }.forEach {
            sortColors(it.printed())
            it shouldEqual ints
        }
    }

    fun sortColors(nums: Array<Int>) {
        var l = 0
        var i = 0
        var r = nums.lastIndex
        while (i <= r) {
            when (nums[i]) {
                0 -> nums.swap(l++, i++)
                1 -> i++
                2 -> nums.swap(i, r--)
            }
        }
    }

    private fun Array<Int>.swap(i1: Int, i2: Int) {
        val tmp = this[i1]
        this[i1] = this[i2]
        this[i2] = tmp
    }
}
