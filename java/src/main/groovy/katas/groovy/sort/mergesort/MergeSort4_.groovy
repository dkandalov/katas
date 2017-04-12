package katas.groovy.sort.mergesort

import org.junit.Test

/**
 * User: DKandalov
 */
class MergeSort4_ {
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 1]) == [1, 1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([2, 4, 3, 1]) == [1, 2, 3, 4]
    assert sort([2, 5, 4, 3, 1]) == [1, 2, 3, 4, 5]

    [1, 2, 3, 4, 5, 6].permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5, 6]
    }
  }

  static List sort(List list) {
    if (list.size() <= 1) return list
    def (l1, l2) = split(list)
    merge(sort(l1), sort(l2))
  }

  static List merge(List list1, List list2) {
    def result = []

    int i1 = 0
    int i2 = 0
    while (i1 < list1.size() && i2 < list2.size()) {
      if (list1[i1] < list2[i2]) {
        result << list1[i1++]
      } else {
        result << list2[i2++]
      }
    }
    while (i1 < list1.size()) { result << list1[i1++] }
    while (i2 < list2.size()) { result << list2[i2++] }

    result
  }

  static def split(List list) {
    def middle = list.size() / 2
    [list[0..middle - 1], list[middle..-1]]
  }
}
