package katas.kotlin.sort.insertionsort

import katas.kotlin.sort.SortingTests
import kotlincommon.swap

class InsertionSort0 : SortingTests({ insertSort(it.toMutableList()) }) {
    companion object {
        fun <T : Comparable<T>> insertSort(list: MutableList<T>): List<T> {
            if (list.size <= 1) return list
            var i = 1
            while (i < list.size) {
                var j = i
                while (j > 0 && list[j - 1] > list[j]) {
                    list.swap(j - 1, j)
                    j--
                }
                i++
            }
            return list
        }
    }
}