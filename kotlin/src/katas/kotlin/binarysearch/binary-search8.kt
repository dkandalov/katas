package katas.kotlin.binarysearch

import kotlincommon.test.shouldEqual
import org.junit.Test

class BinarySearchTest8 {
    @Test fun `find index of an element in a sorted list`() {
        listOf(1)._binarySearch(0) shouldEqual -1
        listOf(1)._binarySearch(1) shouldEqual 0
        listOf(1)._binarySearch(2) shouldEqual -1

        listOf(1, 2)._binarySearch(0) shouldEqual -1
        listOf(1, 2)._binarySearch(1) shouldEqual 0
        listOf(1, 2)._binarySearch(2) shouldEqual 1
        listOf(1, 2)._binarySearch(3) shouldEqual -1

        listOf(1, 2, 3)._binarySearch(0) shouldEqual -1
        listOf(1, 2, 3)._binarySearch(1) shouldEqual 0
        listOf(1, 2, 3)._binarySearch(2) shouldEqual 1
        listOf(1, 2, 3)._binarySearch(3) shouldEqual 2
        listOf(1, 2, 3)._binarySearch(4) shouldEqual -1
    }

    private fun <T : Comparable<T>> List<T>._binarySearch(value: T): Int {
        var from = 0
        var to = size
        while (from < to) {
            val midIndex = (from + to) / 2
            val midValue = this[midIndex]
            when {
                value < midValue -> to = midIndex
                value > midValue -> from = midIndex + 1
                else -> return midIndex
            }
        }
        return -1
    }
}