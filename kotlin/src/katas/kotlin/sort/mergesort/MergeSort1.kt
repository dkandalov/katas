package katas.kotlin.sort.mergesort

import katas.kotlin.sort.checkSortFunction
import org.junit.Test

class MergeSort1 {
    @Test fun `sort list of ints`() {
        checkSortFunction({ it.mergeSort() })
    }

    private fun <T : Comparable<T>> List<T>.mergeSort(): List<T> {
        if (this.size <= 1) return this

        val pivotIndex = size / 2
        return merge(take(pivotIndex).mergeSort(), drop(pivotIndex).mergeSort())
    }

    private fun <T : Comparable<T>> merge(list1: List<T>, list2: List<T>): List<T> =
        if (list1.isEmpty()) list2
        else if (list2.isEmpty()) list1
        else if (list1.first() < list2.first()) list1.take(1) + merge(list1.drop(1), list2)
        else list2.take(1) + merge(list1, list2.drop(1))
}