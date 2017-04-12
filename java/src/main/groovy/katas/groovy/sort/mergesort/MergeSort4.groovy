package katas.groovy.sort.mergesort

import org.junit.Test

/**
 * User: dima
 * Date: 5/2/11
 */
class MergeSort4 {
  @Test
  public void shouldSortList() {
    assert mergeSort([]) == []

    (2..8).asList().inject([1]) { acc, i ->
      acc.permutations().each { assert mergeSort(it) == acc }
      acc + i
    }
  }

  List mergeSort(List list) {
    if (list.size() <= 1) return new ArrayList(list)

    int mid = list.size() / 2
    def left = mergeSort(list.subList(0, mid))
    def right = mergeSort(list.subList(mid, list.size()))

    merge(left, right)
  }

  List merge(List left, List right) {
    def result = []
    int i = 0
    int j = 0
    while (i < left.size() || j < right.size()) {
      if (i == left.size()) {
        result << right[j++]
      } else if (j == right.size()) {
        result << left[i++]
      } else {
        result << (left[i] < right[j] ? left[i++] : right[j++])
      }
    }
    result
  }
}
