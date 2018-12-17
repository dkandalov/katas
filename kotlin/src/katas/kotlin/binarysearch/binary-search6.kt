package katas.kotlin.binarysearch

import kotlincommon.test.shouldEqual
import org.junit.Test

class BinarySearchTests {
    @Test fun `find index of an element in the sorted list`() {
        listOf(1).findIndexOf(0) shouldEqual -1
        listOf(1).findIndexOf(1) shouldEqual 0
        listOf(1).findIndexOf(2) shouldEqual -1

        listOf(1, 2).findIndexOf(0) shouldEqual -1
        listOf(1, 2).findIndexOf(1) shouldEqual 0
        listOf(1, 2).findIndexOf(2) shouldEqual 1
        listOf(1, 2).findIndexOf(3) shouldEqual -1

        listOf(1, 2, 3).findIndexOf(0) shouldEqual -1
        listOf(1, 2, 3).findIndexOf(1) shouldEqual 0
        listOf(1, 2, 3).findIndexOf(2) shouldEqual 1
        listOf(1, 2, 3).findIndexOf(3) shouldEqual 2
        listOf(1, 2, 3).findIndexOf(4) shouldEqual -1
    }
}

private fun <E: Comparable<E>> List<E>.findIndexOf(element: E): Int {
    var from = 0
    var to = size
    while (from < to) {
        val index = (from + to) / 2
        if (element < this[index]) to = index
        else if (this[index] < element) from = index + 1
        else return index
    }
    return -1
}
