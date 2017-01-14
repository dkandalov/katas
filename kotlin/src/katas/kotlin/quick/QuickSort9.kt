package katas.kotlin.quick

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class QuickSort9 {
    fun <T : Comparable<T>> sort(list: List<T>): List<T> {
        if (list.size <= 1) return list

        val pivot = list.first()
        val tail = list.subList(1, list.size)

        return sort(tail.filter { it <= pivot }) +
                pivot +
                sort(tail.filter { it > pivot })
    }

    @Test fun `sort list of integers`() {
        assertThat(sort(emptyList<Int>()), equalTo(emptyList<Int>()))
        assertThat(sort(listOf(1)), equalTo(listOf(1)))
        assertThat(sort(listOf(1, 2)), equalTo(listOf(1, 2)))
        assertThat(sort(listOf(2, 1)), equalTo(listOf(1, 2)))
        assertThat(sort(listOf(3, 2, 1)), equalTo(listOf(1, 2, 3)))
        assertThat(sort(listOf(3, 2, 2, 1)), equalTo(listOf(1, 2, 2, 3)))
        assertThat(sort(listOf(2, 3, 2, 1)), equalTo(listOf(1, 2, 2, 3)))
    }
}