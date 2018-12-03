package katas.kotlin.sort.mergesort

import katas.kotlin.sort.SortingTests

class MergeSort2 : SortingTests(::sort) {
    companion object {
        private fun <T : Comparable<T>> sort(list: List<T>): List<T> {
            if (list.size <= 1) return list

            val i = list.size / 2
            val left = list.subList(0, i)
            val right = list.subList(i, list.size)
            return merge(sort(left), sort(right))
        }

        private fun <T : Comparable<T>> merge(list1: List<T>, list2: List<T>): List<T> {
            val result = ArrayList<T>()
            var i1 = 0
            var i2 = 0
            while (i1 < list1.size && i2 < list2.size) {
                if (list1[i1] < list2[i2]) {
                    result.add(list1[i1])
                    i1++
                } else {
                    result.add(list2[i2])
                    i2++
                }
            }
            while (i1 < list1.size) result.add(list1[i1++])
            while (i2 < list2.size) result.add(list2[i2++])
            return result
        }
    }
}

class MergeSortWithLessAllocation : SortingTests({ sort(it.toMutableList()) }) {
    companion object {
        private fun <T : Comparable<T>> sort(list: MutableList<T>) =
            doSort(list, from = 0, to = list.size, buffer = ArrayList(list))

        private fun <T : Comparable<T>> doSort(list: MutableList<T>, from: Int, to: Int, buffer: MutableList<T>): List<T> {
            if (to - from <= 1) return buffer
            val i = (to + from) / 2
            doSort(list, from, i, buffer)
            doSort(list, i, to, buffer)
            return merge(list, buffer, from, i, to)
        }

        private fun <T : Comparable<T>> merge(list: MutableList<T>, buffer: MutableList<T>, from: Int, mid: Int, to: Int): List<T> {
            var i = from
            var j = mid
            var w = from

            while (i < mid && j < to) buffer[w++] = list[if (list[i] < list[j]) i++ else j++]
            while (i < mid) buffer[w++] = list[i++]
            while (j < to) buffer[w++] = list[j++]
            while (--w >= from) list[w] = buffer[w]

            return list
        }
    }
}