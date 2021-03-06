package katas.kotlin.sort.insertionsort

import katas.kotlin.sort.SortingTests
import nonstdlib.swap

class InsertionSort1 : SortingTests({ sort(it.toMutableList()) }) {
    companion object {
        private fun <T: Comparable<T>> sort(list: MutableList<T>): List<T> {
            if (list.size <= 1) return list

            var i = 1
            while (i < list.size) {
                var j = i
                while (j >= 1 && list[j - 1] > list[j]) {
                    list.swap(j - 1, j)
                    j--
                }
                i++
            }

            return list
        }
    }
}