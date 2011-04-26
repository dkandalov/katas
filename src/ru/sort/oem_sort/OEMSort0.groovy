package ru.sort.oem_sort

import org.junit.Test

/**
 * User: DKandalov
 */
class OEMSort0 {
  @Test
  public void aaa() {
    System.out.println([1, 2, 3, 4][0..1])
    System.out.println([1, 2, 3, 4][2..-1])
  }

  @Test
  public void shouldSortListUsingOddEvenMergeSort() {
//    assert sort([]) == []
//    assert sort([1]) == [1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
//    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([4, 2, 3, 1]) == [1, 2, 3, 4]
//    assert sort([5, 4, 2, 3, 1]) == [1, 2, 3, 4, 5]
//    assert sort([5, 4, 6, 2, 3, 1]) == [1, 2, 3, 4, 5, 6]
  }

  List sort(List list) {
    if (list.size() < 2) return list

    def midPos = list.size() / 2
    merge(sort(list[0..midPos - 1]), sort(list[midPos..-1]))
  }

  List merge(List list1, List list2) {
    if (list1.size() == 1 && list2.size() == 1) {
      return (list1[0] > list2[0]) ? [list2[0], list1[0]] : [list1[0], list2[0]]
    }

    unshuffle(list1, list2)
    merge(list1[0..(list1.size() / 2) - 1], list1[(list1.size() / 2)..-1])
    merge(list2[0..(list2.size() / 2) - 1], list2[(list2.size() / 2)..-1])
    shuffle(list1, list2)

    list1 + list2
  }

  def shuffle(List list, List list2) {
  }

  def unshuffle(List list1, List list) {
  }
}
