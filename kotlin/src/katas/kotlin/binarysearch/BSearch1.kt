package katas.kotlin.binarysearch

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class BSearch1 {

    @Test fun `find index of item in list`() {
        listOf<Int>().let {
            assertThat(it.binarySearch(1), equalTo(-1))
        }

        listOf(1).let {
            assertThat(it.binarySearch(0), equalTo(-1))
            assertThat(it.binarySearch(1), equalTo(0))
            assertThat(it.binarySearch(2), equalTo(-1))
        }

        listOf(1, 2, 3).let {
            assertThat(it.binarySearch(0), equalTo(-1))
            assertThat(it.binarySearch(1), equalTo(0))
            assertThat(it.binarySearch(2), equalTo(1))
            assertThat(it.binarySearch(3), equalTo(2))
            assertThat(it.binarySearch(4), equalTo(-1))
        }
    }

    private fun <T : Comparable<T>> List<T>.binarySearch(value: T): Int {
        var from = 0
        var to = size
        while (from < to) {
            val midIndex = (from + to) / 2
            if (this[midIndex] == value) {
                return midIndex
            } else if (value < this[midIndex]) {
                to = midIndex
            } else {
                from = midIndex + 1
            }
        }
        return -1
    }
}
