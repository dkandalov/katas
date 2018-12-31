package katas.kotlin.sort.mergesort

import kotlincommon.listOfInts
import kotlincommon.permutations
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.random.Random

class MergeSort5Tests {
    @Test fun `trivial examples`() {
        emptyList<Int>().mergeSort() shouldEqual emptyList()
        listOf(1).mergeSort() shouldEqual listOf(1)
    }

    @Test fun `sort list of 2 elements`() {
        listOf(1, 2).mergeSort() shouldEqual listOf(1, 2)
        listOf(2, 1).mergeSort() shouldEqual listOf(1, 2)
    }

    @Test fun `sort list of 3 elements`() {
        listOf(1, 2, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(1, 3, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 1, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 3, 1).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(3, 1, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(3, 2, 1).mergeSort() shouldEqual listOf(1, 2, 3)
    }

    @Test fun `sort list of 4 elements`() {
        listOf(1, 2, 3, 4).permutations().forEach {
            it.mergeSort() shouldEqual listOf(1, 2, 3, 4)
        }
    }

    @Test fun `sort random list`() {
        fun List<Int>.isSorted() = windowed(2).all { it[0] <= it[1] }

        val list = Random.listOfInts(sizeRange = 0..100, valuesRange = 0..100).printed()
        list.mergeSort().isSorted() shouldEqual true
    }
}

private fun <E: Comparable<E>> List<E>.mergeSort(): List<E> {
    fun merge(left: List<E>, right: List<E>): List<E> {
        var i = 0
        var j = 0
        val result = ArrayList<E>()
        while (i < left.size && j < right.size) {
            if (left[i] < right[j]) result.add(left[i++])
            else result.add(right[j++])
        }
        result.addAll(left.drop(i))
        result.addAll(right.drop(j))
        return result
    }

    val queue = LinkedList<List<E>>()
    queue.addAll(map { listOf(it) })
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
    return merge(
        subList(0, size / 2).mergeSort(),
        subList(size / 2, size).mergeSort()
    )
}
