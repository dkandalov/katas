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
