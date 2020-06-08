package katas.kotlin.binarysearch

import datsok.shouldEqual
import org.junit.Test

class BSearch4 {
    @Test fun `find index of an element in a list`() {
        listOf<Int>().binarySearch_(0) shouldEqual -1

        listOf(1).let {
            it.binarySearch_(0) shouldEqual -1
            it.binarySearch_(1) shouldEqual 0
            it.binarySearch_(2) shouldEqual -1
        }

        listOf(1, 2).let {
            it.binarySearch_(0) shouldEqual -1
            it.binarySearch_(1) shouldEqual 0
            it.binarySearch_(2) shouldEqual 1
            it.binarySearch_(3) shouldEqual -1
        }

        listOf(1, 2, 3).let {
            it.binarySearch_(0) shouldEqual -1
            it.binarySearch_(1) shouldEqual 0
            it.binarySearch_(2) shouldEqual 1
            it.binarySearch_(3) shouldEqual 2
            it.binarySearch_(4) shouldEqual -1
        }

        listOf(1, 2, 3, 4).let {
            it.binarySearch_(0) shouldEqual -1
            it.binarySearch_(1) shouldEqual 0
            it.binarySearch_(2) shouldEqual 1
            it.binarySearch_(3) shouldEqual 2
            it.binarySearch_(4) shouldEqual 3
            it.binarySearch_(5) shouldEqual -1
        }
    }

    private fun <E: Comparable<E>> List<E>.binarySearch_(value: E): Int {
        var from = 0
        var to = size

        while (from < to) {
            val midIndex = (from + to) / 2
            val midValue = this[midIndex]

            if (midValue == value) return midIndex
            else if (midValue < value) from = midIndex + 1
            else to = midIndex
        }
        return -1
    }
}
