package katas.kotlin.remove_duplicates_from_array

import kotlincommon.test.shouldEqual
import org.junit.Test

class RemoveDuplicatesFromSortedArrayTests {
    @Test fun `remove the duplicates in-place such that each element appear only once`() {
        intArrayOf(1).removeDuplicates() shouldEqual 1
        intArrayOf(1, 1).removeDuplicates() shouldEqual 1
        intArrayOf(1, 2).removeDuplicates() shouldEqual 2
    }
}

private fun IntArray.removeDuplicates(): Int {
    if (size <= 1) return size
    var i = 0
    var j = 1
    if (this[i] == this[j]) j++ else i++
    if (j == size) return i + 1

    return i + 1
}
