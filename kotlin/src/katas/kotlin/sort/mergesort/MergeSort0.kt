package katas.kotlin.sort.mergesort

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class MergeSort0Test {
    @Test fun `sort list of ints`() {
        assertThat(listOf(1).mergeSort(), equalTo(listOf(1)))

        assertThat(listOf(1, 2).mergeSort(), equalTo(listOf(1, 2)))
        assertThat(listOf(2, 1).mergeSort(), equalTo(listOf(1, 2)))

        assertThat(listOf(1, 2, 3).mergeSort(), equalTo(listOf(1, 2, 3)))
        assertThat(listOf(1, 3, 2).mergeSort(), equalTo(listOf(1, 2, 3)))
        assertThat(listOf(2, 1, 3).mergeSort(), equalTo(listOf(1, 2, 3)))
        assertThat(listOf(2, 3, 1).mergeSort(), equalTo(listOf(1, 2, 3)))
        assertThat(listOf(3, 1, 2).mergeSort(), equalTo(listOf(1, 2, 3)))
        assertThat(listOf(3, 2, 1).mergeSort(), equalTo(listOf(1, 2, 3)))
    }
}

private fun <E: Comparable<E>> List<E>.mergeSort(): List<E> {
    if (size <= 1) return this
    val midIndex = size / 2
    return merge(take(midIndex).mergeSort(), drop(midIndex).mergeSort())
}

fun <E : Comparable<E>> merge(list1: List<E>, list2: List<E>): List<E> {
    return if (list1.isEmpty()) list2
    else if (list2.isEmpty()) list1
    else if (list1.first() < list2.first()) listOf(list1.first()) + merge(list1.drop(1), list2)
    else listOf(list2.first()) + merge(list1, list2.drop(1))
}
