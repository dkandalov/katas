package ru.sort.insertsort

import org.junit.Test

class InsertSort11 {
  @Test void "sorting list"() {
    assert sorted([]) == []
    assert sorted([1]) == [1]
    assert sorted([1, 2]) == [1, 2]
    assert sorted([2, 1]) == [1, 2]
    assert sorted([3, 1, 2]) == [1, 2, 3]
    [1, 2, 3].permutations().each {
      assert sorted(it) == [1, 2, 3]
    }
  }

  private static List sorted(List list) {
    def result = []
    for (def item : list) {
      int i = result.size()
      while (i > 0 && item < result[i - 1]) i-- // was comparing with list[i - 1]
      result.add(i, item)
    }
    result
  }
}
