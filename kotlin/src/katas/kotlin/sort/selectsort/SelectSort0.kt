package katas.kotlin.sort.selectsort

import katas.kotlin.sort.SortingTests
import katas.kotlin.swap

class SelectSort0 : SortingTests({ sort(it.toMutableList()) })

private fun <T : Comparable<T>> sort(list: MutableList<T>): List<T> {
    list.indices.forEach { i ->
        val minIndex = IntRange(i, list.lastIndex).minBy { list[it] }!!
        list.swap(minIndex, i)
    }
    return list
}

