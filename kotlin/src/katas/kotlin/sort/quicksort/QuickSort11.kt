package katas.kotlin.sort.quicksort

import katas.kotlin.sort.SortingTests
import nonstdlib.swap

class QuickSort11 : SortingTests({ sort(it.toMutableList()) }) {
    companion object {
        private fun <T : Comparable<T>> sort(list: MutableList<T>, from: Int = 0, to: Int = list.size): List<T> {
            if (to - from <= 1) return list
            val midIndex = lomutoPartition(list, from, to)
            sort(list, from, midIndex)
            sort(list, midIndex + 1, to)
            return list
        }

        /**
         * https://en.wikipedia.org/wiki/Quicksort#Lomuto_partition_scheme
         */
        private fun <T : Comparable<T>> lomutoPartition(list: MutableList<T>, from: Int, to: Int): Int {
            val pivotIndex = to - 1
            var i = from
            var midIndex = from

            while (i < pivotIndex) {
                if (list[i] < list[pivotIndex]) {
                    list.swap(i, midIndex)
                    midIndex++
                }
                i++
            }
            list.swap(midIndex, pivotIndex)

            return midIndex
        }
    }
}