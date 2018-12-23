package katas.kotlin.sort.mergesort

import katas.kotlin.sort.SortingTests

class MergeSort0: SortingTests({ it.mergeSort() }) {
    companion object {
        private fun <E: Comparable<E>> List<E>.mergeSort(): List<E> {
            if (size <= 1) return this
            val midIndex = size / 2
            return merge(
                take(midIndex).mergeSort(),
                drop(midIndex).mergeSort()
            )
        }

        private fun <E: Comparable<E>> merge(list1: List<E>, list2: List<E>): List<E> {
            return if (list1.isEmpty()) list2
            else if (list2.isEmpty()) list1
            else if (list1.first() < list2.first()) listOf(list1.first()) + merge(list1.drop(1), list2)
            else listOf(list2.first()) + merge(list1, list2.drop(1))
        }
    }
}
