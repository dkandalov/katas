package katas.kotlin.sort.quicksort

import katas.kotlin.sort.SortingTests
import kotlincommon.swap

class QuickSort10 : SortingTests({ quickSort(it.toMutableList()) }) {
    companion object {
        private fun <T : Comparable<T>> quickSort(list: MutableList<T>, from: Int = 0, to: Int = list.size - 1): MutableList<T> {
            if (from >= to) return list

            val i = almostHoarePartition(list, from, to)
            quickSort(list, from, i - 1)
            quickSort(list, i, to)

            return list
        }

        /**
         * https://en.wikipedia.org/wiki/Quicksort#Hoare_partition_scheme
         */
        private fun <T : Comparable<T>> almostHoarePartition(list: MutableList<T>, from: Int, to: Int): Int {
            val pivot = list[from]
            var i = from
            var j = to
            while (i <= j) {
                while (list[i] < pivot) i++
                while (list[j] > pivot) j--
                if (i <= j) list.swap(i++, j--)
            }
            return i
        }

        private fun <E: Comparable<E>> hoarePartition(list: MutableList<E>, from: Int, to: Int): Int {
            val pivot = list[from]
            var i = from
            var j = to
            while (true) {
                while (list[i] < pivot) i++
                while (list[j] > pivot) j--
                if (i < j) list.swap(i++, j--)
                else return j
            }
        }
    }
}