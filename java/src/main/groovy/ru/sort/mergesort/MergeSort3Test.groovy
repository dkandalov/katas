package ru.sort.mergesort

import org.junit.Test

/**
 * User: dima
 * Date: 5/2/11
 */
class MergeSort3Test {
  @Test
  public void shouldSortList() {
    assert mergeSort([]) == []
    assert mergeSort([1]) == [1]
    assert mergeSort([1, 2]) == [1, 2]
    assert mergeSort([2, 1]) == [1, 2]
    [1, 2, 3].permutations().each { assert mergeSort(it) == [1, 2, 3] }

    (2..8).asList().inject([1]) { acc, i ->
      acc.permutations().each { assert mergeSort(it) == acc }
      acc + i
    }
  }
}
