package katas.kotlin.binarysearch

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class BSearch2Test {
    @Test fun `find position of an element in a list`() {
        assertThat(listOf<Int>().search(1), equalTo(-1))

        assertThat(
            (0..4).map{ listOf(1, 2, 3).search(it) },
            equalTo(listOf(-1, 0, 1, 2, -1))
        )
    }

    fun <T: Comparable<T>> Collection<T>.search(item: T, shift: Int = 0): Int {
        if (isEmpty()) return -1

        val midIndex = size / 2
        val midValue = drop(midIndex).first()
        if (midValue == item) {
            return midIndex + shift
        } else if (midValue > item) {
            return take(midIndex).search(item, shift)
        } else {
            return drop(midIndex + 1).search(item, shift + midIndex + 1)
        }
    }
}