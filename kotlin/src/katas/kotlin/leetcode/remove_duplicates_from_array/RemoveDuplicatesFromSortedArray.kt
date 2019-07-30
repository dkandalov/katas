package katas.kotlin.leetcode.remove_duplicates_from_array

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/remove-duplicates-from-sorted-array/
 */
class RemoveDuplicatesFromSortedArrayTests {
    @Test fun `remove the duplicates in-place such that each element appear only once`() {
        intArrayOf().removeDuplicatesToList() shouldEqual emptyList()
        intArrayOf(1).removeDuplicatesToList() shouldEqual listOf(1)
        intArrayOf(1).removeDuplicatesToList() shouldEqual listOf(1)
        intArrayOf(1, 1).removeDuplicatesToList() shouldEqual listOf(1)
        intArrayOf(1, 1, 1).removeDuplicatesToList() shouldEqual listOf(1)

        intArrayOf(1, 2).removeDuplicatesToList() shouldEqual listOf(1, 2)
        intArrayOf(1, 1, 2).removeDuplicatesToList() shouldEqual listOf(1, 2)
        intArrayOf(1, 2, 2).removeDuplicatesToList() shouldEqual listOf(1, 2)

        intArrayOf(1, 2, 3).removeDuplicatesToList() shouldEqual listOf(1, 2, 3)
        intArrayOf(1, 1, 2, 3).removeDuplicatesToList() shouldEqual listOf(1, 2, 3)
        intArrayOf(1, 2, 2, 3).removeDuplicatesToList() shouldEqual listOf(1, 2, 3)
        intArrayOf(1, 2, 3, 3).removeDuplicatesToList() shouldEqual listOf(1, 2, 3)
        intArrayOf(1, 1, 2, 2, 3, 3).removeDuplicatesToList() shouldEqual listOf(1, 2, 3)

        intArrayOf(0, 0, 1, 1, 1, 2, 2, 3, 3, 4).removeDuplicatesToList() shouldEqual listOf(0, 1, 2, 3, 4)
    }
}

private fun IntArray.removeDuplicatesToList(): List<Int> {
    val newSize = removeDuplicates()
    return toList().take(newSize)
}

private fun IntArray.removeDuplicates(): Int {
    if (size <= 1) return size
    var i = 0
    var j = 1

    while (j < size) {
        if (this[i] != this[j]) {
            if (i + 1 != j) this[i + 1] = this[j]
            i++
        }
        j++
    }

    return i + 1
}
