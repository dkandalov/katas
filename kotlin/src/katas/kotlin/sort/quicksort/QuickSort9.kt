package katas.kotlin.sort.quicksort

import katas.kotlin.sort.checkSortFunction
import org.junit.Test

class QuickSort9 {
    private fun <T : Comparable<T>> sort(list: List<T>): List<T> {
        if (list.size <= 1) return list

        val pivot = list.first()
        val tail = list.subList(1, list.size)

        return sort(tail.filter { it <= pivot }) +
                pivot +
                sort(tail.filter { it > pivot })
    }

    @Test fun `sort list of integers`() {
        checkSortFunction(::sort)
    }
}