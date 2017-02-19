package katas.kotlin.quicksort

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class QuickSort10 {
    fun <T : Comparable<T>> sort(list: MutableList<T>, from: Int = 0, to: Int = list.size - 1): MutableList<T> {
        if (from >= to) return list

        val i = partition(list, from, to)
        sort(list, from, i - 1)
        sort(list, i, to)
        
        return list
    }

    private fun <T : Comparable<T>> partition(list: MutableList<T>, from: Int, to: Int): Int {
        val pivot = list[from]
        var i = from
        var j = to
        while (i <= j) {
            while (list[i] < pivot) i++
            while (list[j] > pivot) j--
            if (i <= j) {
                swap(list, i, j)
                i++
                j--
            }
        }
        return i
    }

    private fun <T> swap(list: MutableList<T>, i: Int, j: Int) {
        val tmp = list[i]
        list[i] = list[j]
        list[j] = tmp
    }

    @Test fun `sort list of integers`() {
        assertThat(sort(mutableListOf<Int>()), equalTo(emptyList<Int>()))
        assertThat(sort(mutableListOf(1)), equalTo(listOf(1)))
        assertThat(sort(mutableListOf(1, 2)), equalTo(listOf(1, 2)))
        assertThat(sort(mutableListOf(2, 1)), equalTo(listOf(1, 2)))
        assertThat(sort(mutableListOf(3, 2, 1)), equalTo(listOf(1, 2, 3)))
        assertThat(sort(mutableListOf(3, 2, 2, 1)), equalTo(listOf(1, 2, 2, 3)))
        assertThat(sort(mutableListOf(2, 3, 2, 1)), equalTo(listOf(1, 2, 2, 3)))
    }
}