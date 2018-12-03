package katas.kotlin.sort.quicksort

import katas.kotlin.sort.SortingTests

class QuickSort9 : SortingTests(::sort) {
    companion object {
        private fun <T : Comparable<T>> sort(list: List<T>): List<T> {
            if (list.size <= 1) return list

            val pivot = list.first()
            val tail = list.subList(1, list.size)

            return sort(tail.filter { it <= pivot }) +
                pivot +
                sort(tail.filter { it > pivot })
        }
    }
}