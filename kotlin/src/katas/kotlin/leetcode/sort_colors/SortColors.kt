package katas.kotlin.leetcode.sort_colors

import datsok.shouldEqual
import nonstdlib.permutationsSequence
import nonstdlib.printed
import org.junit.jupiter.api.Test

/**
 * https://leetcode.com/problems/sort-colors
 */
class SortColors {
    @Test fun `it works`() {
        listOf(0, 1, 2).permutationsSequence().forEach { list ->
            list.toIntArray().let {
                sortColors(it.printed())
                it shouldEqual intArrayOf(0, 1, 2)
            }
        }
        listOf(0, 0, 1, 1, 2, 2).permutationsSequence().forEach { list ->
            list.toIntArray().let {
                sortColors(it.printed())
                it shouldEqual intArrayOf(0, 0, 1, 1, 2, 2)
            }
        }
    }

    fun sortColors(nums: IntArray) {
        var left = 0
        var right = nums.lastIndex
        var i = left
        while (i <= right) {
            when {
                nums[i] == 0 -> nums.swap(i++, left++)
                nums[i] == 2 -> nums.swap(i, right--)
                else         -> i++
            }
        }
    }
}

private fun IntArray.swap(i1: Int, i2: Int) {
    val temp = this[i1]
    this[i1] = this[i2]
    this[i2] = temp
}
