package katas.kotlin.sort.mergesort

import katas.kotlin.sort.SortingTests

class MergeSort1 : SortingTests({ it.mergeSort() }) {
    companion object {
        private fun <T : Comparable<T>> List<T>.mergeSort(): List<T> {
            if (this.size <= 1) return this
            val pivotIndex = size / 2
            return merge(take(pivotIndex).mergeSort(), drop(pivotIndex).mergeSort())
        }

        private fun <T : Comparable<T>> merge(list1: List<T>, list2: List<T>): List<T> =
            when {
                list1.isEmpty()               -> list2
                list2.isEmpty()               -> list1
                list1.first() < list2.first() -> list1.take(1) + merge(list1.drop(1), list2)
                else                          -> list2.take(1) + merge(list1, list2.drop(1))
            }

    }
}