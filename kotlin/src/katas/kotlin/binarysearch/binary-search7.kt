package katas.kotlin.binarysearch

import kotlincommon.test.shouldEqual
import org.junit.Test

class BinarySearchTests7 {
    @Test fun `find index of an element`() {
        listOf(1).binarySearch_(0) shouldEqual -1
        listOf(1).binarySearch_(1) shouldEqual 0
        listOf(1).binarySearch_(2) shouldEqual -1

        listOf(1, 2).binarySearch_(0) shouldEqual -1
        listOf(1, 2).binarySearch_(1) shouldEqual 0
        listOf(1, 2).binarySearch_(2) shouldEqual 1
        listOf(1, 2).binarySearch_(3) shouldEqual -1
    }
}

private fun <E : Comparable<E>> List<E>.binarySearch_(item: E): Int {
    var from = 0
    var to = size
    while (from < to) {
        val i = (from + to) / 2
        when {
            item < this[i] -> to = i
            item > this[i] -> from = i + 1
            else -> return i
        }
    }
    return -1
}
