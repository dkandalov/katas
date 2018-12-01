package katas.kotlin.sort.quicksort

import katas.kotlin.sort.SortingTests

class QuickSort10 : SortingTests({ quickSort(it.toMutableList()) }) {
    companion object {
        private fun <T : Comparable<T>> quickSort(list: MutableList<T>, from: Int = 0, to: Int = list.size - 1): MutableList<T> {
            if (from >= to) return list

            val i = partition(list, from, to)
            quickSort(list, from, i - 1)
            quickSort(list, i, to)

            return list
        }

        private fun <T : Comparable<T>> partition(list: MutableList<T>, from: Int, to: Int): Int {
            val pivot = list[from]
            var i = from
            var j = to
            while (i <= j) {
                while (list[i] < pivot) i++
                while (list[j] > pivot) j--
                if (i <= j) {
                    swap(list, i, j)
                    i++
                    j--
                }
            }
            return i
        }

        private fun <T> swap(list: MutableList<T>, i: Int, j: Int) {
            val tmp = list[i]
            list[i] = list[j]
            list[j] = tmp
        }
    }
}