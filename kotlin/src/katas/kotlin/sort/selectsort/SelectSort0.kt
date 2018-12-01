package katas.kotlin.sort.selectsort

import katas.kotlin.sort.checkSortFunction
import katas.kotlin.swap
import org.junit.Test

class SelectSort0 {
    @Test fun `select sort`() {
        checkSortFunction { sort(it.toMutableList()) }
    }

    private fun <T : Comparable<T>> sort(list: MutableList<T>): List<T> {
        list.indices.forEach { i ->
            val minIndex = IntRange(i, list.lastIndex).minBy { list[it] }!!
            list.swap(minIndex, i)
        }
        return list
    }
}
