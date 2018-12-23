package katas.kotlin.sort.mergesort

import kotlincommon.listOfInts
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.random.Random

class MergeSortTests3 {
    @Test fun `trivial examples`() {
        emptyList<Int>().mergeSort() shouldEqual emptyList()
        listOf(1).mergeSort() shouldEqual listOf(1)
    }

    @Test fun `basic examples`() {
        listOf(1, 2).mergeSort() shouldEqual listOf(1, 2)
        listOf(2, 1).mergeSort() shouldEqual listOf(1, 2)

        listOf(1, 2, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(1, 3, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 1, 3).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(2, 3, 1).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(3, 1, 2).mergeSort() shouldEqual listOf(1, 2, 3)
        listOf(3, 2, 1).mergeSort() shouldEqual listOf(1, 2, 3)
    }

    @Test fun `sort random list`() {
        fun List<Int>.isSorted() =
            windowed(size = 2, step = 1).all { it[0] <= it[1] }

        val randomList = Random.listOfInts(
            sizeRange = IntRange(0, 100),
            valuesRange = IntRange(0, 100)
        )
        randomList.mergeSort().isSorted().printed() shouldEqual true
    }
}


private fun <E: Comparable<E>> List<E>.mergeSort(): List<E> {
    if (size <= 1) return this
    val midIndex = size / 2
    return merge(
        left = subList(0, midIndex).mergeSort(),
        right = subList(midIndex, size).mergeSort()
    )
}

private fun <E: Comparable<E>> merge(left: List<E>, right: List<E>): List<E> {
    return when {
        left.isEmpty()     -> right
        right.isEmpty()    -> left
        left[0] < right[0] -> listOf(left[0]) + merge(left.drop(1), right)
        else               -> listOf(right[0]) + merge(right.drop(1), left)
    }
}
