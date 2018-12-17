package katas.kotlin.binarysearch

import kotlincommon.test.shouldEqual
import org.junit.Test

class BSearch3 {
    @Test fun `find index of an element in list`() {
        binarySearch(0, emptyList()) shouldEqual -1

        binarySearch(0, listOf(1)) shouldEqual -1
        binarySearch(1, listOf(1)) shouldEqual 0
        binarySearch(2, listOf(1)) shouldEqual -1

        binarySearch(0, listOf(1, 2)) shouldEqual -1
        binarySearch(1, listOf(1, 2)) shouldEqual 0
        binarySearch(2, listOf(1, 2)) shouldEqual 1
        binarySearch(3, listOf(1, 2)) shouldEqual -1

        binarySearch(0, listOf(1, 2, 3)) shouldEqual -1
        binarySearch(1, listOf(1, 2, 3)) shouldEqual 0
        binarySearch(2, listOf(1, 2, 3)) shouldEqual 1
        binarySearch(3, listOf(1, 2, 3)) shouldEqual 2
        binarySearch(4, listOf(1, 2, 3)) shouldEqual -1

        binarySearch(0, listOf(1, 2, 3, 4)) shouldEqual -1
        binarySearch(1, listOf(1, 2, 3, 4)) shouldEqual 0
        binarySearch(2, listOf(1, 2, 3, 4)) shouldEqual 1
        binarySearch(3, listOf(1, 2, 3, 4)) shouldEqual 2
        binarySearch(4, listOf(1, 2, 3, 4)) shouldEqual 3
        binarySearch(5, listOf(1, 2, 3, 4)) shouldEqual -1
    }
    
    private fun <T : Comparable<T>> binarySearch(value: T, list: List<T>): Int {
        var fromIndex = 0
        var toIndex = list.size
        while (fromIndex < toIndex) {
            val midIndex = (toIndex + fromIndex) / 2
            val midValue = list[midIndex]
            if (value == midValue) return midIndex
            else if (value < midValue) toIndex = midIndex
            else if (value > midValue) fromIndex = midIndex + 1
        }
        return -1
    }
}