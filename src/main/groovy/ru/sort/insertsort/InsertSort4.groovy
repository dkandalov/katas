package ru.sort.insertsort

import org.junit.Test

/**
 * User: DKandalov
 */
class InsertSort4 {
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 1]) == [1, 1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([2, 4, 3, 1]) == [1, 2, 3, 4]

    [1, 2, 3, 4, 5].permutations().each {
      assert sort(it) == [1, 2, 3, 4, 5]
    }
  }

  private static def sort(list) {
    list.size().times { i ->
      for (int j = i - 1; j >= 0; j--) {
        if (!swapIfLess(list, j, j + 1)) break // was swapIfLess(list, j, __i__)
      }
    }
    list
  }

  private static def swapIfLess(List list, i1, i2) {
    if (list[i1] > list[i2]) {
      def tmp = list[i1]
      list[i1] = list[i2]
      list[i2] = tmp
      true
    } else {
      false
    }
  }
}
