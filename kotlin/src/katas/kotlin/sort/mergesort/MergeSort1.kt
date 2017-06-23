package katas.kotlin.sort.mergesort

import katas.kotlin.shouldEqual
import org.junit.Test

class MergeSort1 {
    @Test fun `sort list of ints`() {
        emptyList<Int>().mergeSort() shouldEqual emptyList<Int>()
        listOf(1).mergeSort() shouldEqual listOf(1)

        listOf(1, 2).mergeSort() shouldEqual listOf(1, 2)
        listOf(2, 1).mergeSort() shouldEqual listOf(1, 2)

        listOf(1, 2, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(1, 3, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 1, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 3, 1).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(3, 1, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(3, 2, 1).mergeSort() shouldEqual listOf(1, 2, 3)

        listOf(1, 2, 3, 2).mergeSort() shouldEqual listOf(1, 2, 2, 3)
    }

    private fun <T: Comparable<T>> List<T>.mergeSort(): List<T> {
        if (this.size <= 1) return this

        val pivotIndex = size / 2
        val pivot = this[pivotIndex]
        val left = filterIndexed { index, n -> n <= pivot && index != pivotIndex }
        val right = filterIndexed { index, n -> n > pivot && index != pivotIndex }

        return merge(left.mergeSort() + pivot, right.mergeSort())
    }

    private fun <T: Comparable<T>> merge(list1: List<T>, list2: List<T>): List<T> =
        if (list1.isEmpty()) list2
        else if (list2.isEmpty()) list1
        else if (list1.first() < list2.first()) list1.take(1) + merge(list1.drop(1), list2)
        else list2.take(1) + merge(list1, list2.drop(1))
}