package ru.sort.mergesort

import org.junit.Test
import katas.java.util.Pomodoro

/**
 * User: dima
 * Date: 01/03/2012
 */
@Pomodoro("<1")
class MergeSort9 {
  @Test public void shouldSortListOfElementUsingMergeSort() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1,2]
    assert sort([3, 1, 2]) == [1, 2, 3]
    (1..5).toList().permutations().each { list ->
      assert sort(list) == (1..5).toList()
    }
  }

  static sort(list) {
    if (list.size() <= 1) return list
    def parts = split(list)
    merge(sort(parts[0]), sort(parts[1]))
  }

  static List split(list) {
    def mid = list.size().intdiv(2)
    [list[0..<mid], list[mid..-1]]
  }

  static List merge(list1, list2) {
    def result = []
    int i1 = 0
    int i2 = 0
    while (i1 < list1.size() && i2 < list2.size()) {
      if (list1[i1] <= list2[i2]) {
        result << list1[i1]
        i1++
      } else {
        result << list2[i2]
        i2++
      }
    }
    for (int i = i1; i < list1.size(); i++) result << list1[i]
    for (int i = i2; i < list2.size(); i++) result << list2[i]
    result
  }

  static List merge_recursive(list1, list2) {
    if (list1.empty) return list2
    if (list2.empty) return list1
    if (list1[0] < list2[0]) [list1[0]] + merge(list1.tail(), list2)
    else [list2[0]] + merge(list1, list2.tail())
  }
}
