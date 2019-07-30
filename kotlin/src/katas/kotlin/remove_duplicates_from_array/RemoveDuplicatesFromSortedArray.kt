package katas.kotlin.remove_duplicates_from_array

import kotlincommon.test.shouldEqual
import org.junit.Test

class RemoveDuplicatesFromSortedArrayTests {
    @Test fun `remove the duplicates in-place such that each element appear only once`() {
        intArrayOf().let {
            it.removeDuplicates() shouldEqual 0
            it shouldEqual intArrayOf()
        }
        intArrayOf(1).removeDuplicates() shouldEqual 1
        intArrayOf(1, 1).removeDuplicates() shouldEqual 1
        intArrayOf(1, 2).removeDuplicates() shouldEqual 2
        intArrayOf(1, 2, 2).removeDuplicates() shouldEqual 2

        intArrayOf(1, 2, 3).removeDuplicates() shouldEqual 3
        intArrayOf(1, 1, 2, 3).removeDuplicates() shouldEqual 3
        intArrayOf(1, 2, 2, 3).removeDuplicates() shouldEqual 3
        intArrayOf(1, 2, 3, 3).removeDuplicates() shouldEqual 3
    }
}

private fun IntArray.removeDuplicates(): Int {
    if (size <= 1) return size
    var i = 0
    var j = 1

    while (j < size) {
        if (this[i] == this[j]) j++
        else {
            i++
            j++
        }
    }

    return i + 1
}
