package katas.kotlin.remove_duplicates_from_array

import kotlincommon.test.shouldEqual
import org.junit.Test

class RemoveDuplicatesFromSortedArrayTests {
    @Test fun `remove the duplicates in-place such that each element appear only once`() {
        intArrayOf(1).removeDuplicates() shouldEqual 1
    }
}

private fun IntArray.removeDuplicates(): Int {
    return 1
}
