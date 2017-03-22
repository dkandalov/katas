package ru.sort.mergesort

import org.junit.Test

/**
 * User: dima
 * Date: 3/2/11
 */
class MergeSort2 {
  @Test
  public void shouldSort() {
    assert mergeSort([]) == []
    assert mergeSort([1]) == [1]
    assert mergeSort([1, 2]) == [1, 2]
    assert mergeSort([2, 1]) == [1, 2]
    [1, 2, 3].permutations().each { assert mergeSort(it) == [1, 2, 3] }

    (2..5).asList().inject([1]) { acc, i ->
      acc.permutations().each { assert mergeSort(it) == acc }
      acc + i
    }
  }

  def mergeSort(def list) {
    mergeSort(list, 0, list.size())
  }

  def mergeSort(def list, int from, int to) {
    if (to - from <= 1) return list

    int mid = (from + to) / 2
    mergeSort(list, from, mid)
    mergeSort(list, mid, to) // was from instead of to
    merge(list, from, mid, to)

    list
  }

  def merge(def list, int from, int mid, int to) {
    int i = from
    int j = mid
    def tmp = (from..to - 1).collect {0} // didn't fill tmp with values

    for (int k = 0; k < tmp.size(); k++) {
      if (i >= mid) {
        tmp[k] = list[j++]
      } else if (j >= to) {
        tmp[k] = list[i++]
      } else {
        tmp[k] = (list[i] < list[j] ? list[i++] : list[j++])
      }
    }

    (to - from).times { list[from + it] = tmp[it] } // forgot that tmp[] indexes start from 0, used indexing for list instead
  }
}
