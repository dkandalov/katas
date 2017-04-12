package katas.groovy.sort.mergesort

import org.junit.Test

/**
 * @author DKandalov
 */
class MergeSort7 {
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([1, 2, 3]) == [1, 2, 3]
    assert sort([2, 3, 1]) == [1, 2, 3]

    [1, 2, 3, 4, 5].permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5] // forgot to call sort(list) :(
    }
  }

  static def sort(List list) {
    if (list.size() < 2) return list
    int midPos = list.size() / 2
    merge(sort(list.subList(0, midPos)), sort(list.subList(midPos, list.size())))
  }

  static def merge(list1, list2) {
    if (list1.empty) return list2
    if (list2.empty) return list1
    if (list1[0] < list2[0])
      return [list1[0], merge(list1.subList(1, list1.size()), list2)].flatten()
    else
      return [list2[0], merge(list1, list2.subList(1, list2.size()))].flatten()
  }

  static def merge_imperative(list1, list2) {
    def result = []
    int i1 = 0
    int i2 = 0
    while (i1 < list1.size() && i2 < list2.size()) { // used to for list emptiness without removing elements
      if (list1[i1] < list2[i2]) {
        result << list1[i1++]
      } else {
        result << list2[i2++]
      }
    }
    for (int i = i1; i < list1.size(); i++) result << list1[i]
    for (int i = i2; i < list2.size(); i++) result << list2[i]
    result
  }
}
