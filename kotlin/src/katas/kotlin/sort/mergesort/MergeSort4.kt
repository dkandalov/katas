package katas.kotlin.sort.mergesort

import katas.kotlin.permutations
import katas.kotlin.printed
import kotlincommon.listOfInts
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList
import kotlin.random.Random

class MergeSortTests {
    @Test fun `trivial examples`() {
        emptyList<Int>().mergeSort() shouldEqual emptyList()
        listOf(1).mergeSort() shouldEqual listOf(1)
    }

    @Test fun `sort list with two elements`() {
        listOf(1, 2).mergeSort() shouldEqual listOf(1, 2)
        listOf(2, 1).mergeSort() shouldEqual listOf(1, 2)
    }

    @Test fun `sort list with three elements`() {
        listOf(1, 2, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(1, 3, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 1, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 3, 1).mergeSort() shouldEqual listOf(1, 2, 3)
    }

    @Test fun `sort list with four elements`() {
        listOf(1, 2, 3, 4).permutations().forEach {
            it.mergeSort() shouldEqual listOf(1, 2, 3, 4)
        }
    }

    @Test fun `sort random list`() {
        fun List<Int>.isSorted() = windowed(size = 2).all { it[0] <= it[1] }

        val list = Random.listOfInts(
            sizeRange = IntRange(0, 100),
            valuesRange = IntRange(0, 100)
        ).printed()

        val sortedList = list.mergeSort().printed()
        sortedList.isSorted() shouldEqual true
        sortedList.size shouldEqual list.size
    }


    private fun <E: Comparable<E>> List<E>.mergeSort(): List<E> {
        fun merge(left: List<E>, right: List<E>): List<E> {
            val result = ArrayList<E>()
            var i = 0
            var j = 0
            while (i < left.size && j < right.size) {
                result.add(if (left[i] < right[j]) left[i++] else right[j++])
            }
            while (i < left.size) result.add(left[i++])
            while (j < right.size) result.add(right[j++])
            return result
        }

        val queue = LinkedList<List<E>>()
        queue.addAll(this.map { listOf(it) })

        while (queue.size > 1) {
            val list1 = queue.removeFirst()
            val list2 = queue.removeFirst()
            queue.add(merge(list1, list2))
        }
        return queue.firstOrNull() ?: emptyList()
    }

    private fun <E: Comparable<E>> List<E>.mergeSort_recursive(): List<E> {
        fun merge(left: List<E>, right: List<E>): List<E> {
            return when {
                left.isEmpty()     -> right
                right.isEmpty()    -> left
                left[0] < right[0] -> listOf(left[0]) + merge(left.drop(1), right)
                else               -> listOf(right[0]) + merge(left, right.drop(1))
            }
        }

        if (size <= 1) return this
        val midIndex = size / 2
        return merge(
            left = subList(0, midIndex).mergeSort_recursive(),
            right = subList(midIndex, size).mergeSort_recursive()
        )
    }
}
