package katas.kotlin.binarysearch

import kotlincommon.test.shouldEqual
import org.junit.Test

class BinarySearch5Tests {
    @Test fun `find index of item in a sorted list`() {
        listOf(1).search(0) shouldEqual -1
        listOf(1).search(1) shouldEqual 0
        listOf(1).search(2) shouldEqual -1

        listOf(1, 2, 3, 4).search(0) shouldEqual -1
        listOf(1, 2, 3, 4).search(1) shouldEqual 0
        listOf(1, 2, 3, 4).search(2) shouldEqual 1
        listOf(1, 2, 3, 4).search(3) shouldEqual 2
        listOf(1, 2, 3, 4).search(4) shouldEqual 3
        listOf(1, 2, 3, 4).search(5) shouldEqual -1
    }
}

private fun <E: Comparable<E>> List<E>.search(item: E): Int {
    var from = 0
    var to = size
    while (from < to) {
        val i = (from + to) / 2
        if (item == this[i]) return i
        else if (item < this[i]) to = i
        else if (item > this[i]) from = i + 1
    }
    return -1
}
