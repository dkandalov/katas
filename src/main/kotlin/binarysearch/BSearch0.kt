package binarysearch

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class BSearch0 {
    @Test fun `find index of an item`() {
        assertThat(listOf<Int>().binarySearch(0), equalTo(-1))

        assertThat(listOf(0).binarySearch(-1), equalTo(-1))
        assertThat(listOf(0).binarySearch(0), equalTo(0))
        assertThat(listOf(0).binarySearch(1), equalTo(-1))

        assertThat(listOf(0, 1).binarySearch(-1), equalTo(-1))
        assertThat(listOf(0, 1).binarySearch(0), equalTo(0))
        assertThat(listOf(0, 1).binarySearch(1), equalTo(1))
        assertThat(listOf(0, 1).binarySearch(2), equalTo(-1))

        assertThat(listOf(0, 1, 2).binarySearch(-1), equalTo(-1))
        assertThat(listOf(0, 1, 2).binarySearch(0), equalTo(0))
        assertThat(listOf(0, 1, 2).binarySearch(1), equalTo(1))
        assertThat(listOf(0, 1, 2).binarySearch(2), equalTo(2))
        assertThat(listOf(0, 1, 2).binarySearch(3), equalTo(-1))
    }

    private fun <T : Comparable<T>> List<T>.binarySearch(value: T): Int {
        if (this.isEmpty()) return -1

        var from = 0
        var to = this.size

        while (from < to) {
            val midIndex = (from + to) / 2
            val midValue = this[midIndex]
            if (value == midValue) {
                return midIndex
            } else if (value < midValue) {
                to = midIndex
            } else {
                from = midIndex + 1
            }
        }
        return -1
    }

    private fun <T : Comparable<T>> List<T>.binarySearch2(value: T, shift: kotlin.Int = 0): Int {
        if (this.isEmpty()) return -1

        val midIndex = this.size / 2
        val midValue = this[midIndex]
        if (value == midValue) {
            return midIndex + shift
        } else if (value < midValue) {
            return subList(0, midIndex).binarySearch(value, shift)
        } else {
            return subList(midIndex + 1, this.size).binarySearch(value, shift + midIndex + 1)
        }
    }
}