package ru.sort.oemsort

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
//    assert sort([1, 2]) == [1, 2]
//    assert sort([2, 1]) == [1, 2]
//    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([4, 2, 3, 1]) == [1, 2, 3, 4]
//    assert sort([5, 4, 2, 3, 1]) == [1, 2, 3, 4, 5]
    assert sort([5, 4, 6, 8, 7, 2, 3, 1]) == [1, 2, 3, 4, 5, 6, 7, 8]
  }

  List sort(List list) {
    if (list.size() <= 1) return list

    def midPos = list.size() / 2
    merge(sort(list[0..midPos - 1]), sort(list[midPos..-1]))
  }

  List merge(List list1, List list2) {
    if (list1.size() == 1 && list2.size() == 1) {
      return (list1[0] > list2[0]) ? [list2[0], list1[0]] : [list1[0], list2[0]]
    }

//    split(list1, list2)
    def list1Mid = list1.size() / 2
    def list2Mid = list2.size() / 2
    list1 = merge(unshuffle(list1[0..list1Mid - 1], list1[list1Mid..-1]))
    list2 = merge(unshuffle(list2[0..list2Mid - 1], list2[list2Mid..-1]))
    shuffle(list1, list2)

    list1 + list2
  }

  def shuffle(List list1, List list2) { // tried using transpose().. didn't work.. was getting NPE.. didn't investigate
    def shuffled = []
    0.step(list1.size(), 1) { shuffled << list1[it] << list2[it] }
    list1.clear()
    list1.addAll(shuffled[0..(shuffled.size() / 2) - 1])
    list2.clear()
    list2.addAll(shuffled[(shuffled.size() / 2)..-1]) // used shuffled.size() as end of range.. didn't work
    list1 + list2
  }

  def unshuffle(List list1, List list2) {
    def lists = list1 + list2
    list1.clear()
    list2.clear()
    0.step(lists.size(), 2) { list1 << lists[it] }
    1.step(lists.size(), 2) { list2 << lists[it] }
    [list1, list2]
  }

  @Test
  public void shouldShuffleLists() {
    assert shuffle([1], [2]) == [1, 2]
    assert shuffle([1, 2], [3, 4]) == [1, 3, 2, 4]
    assert shuffle([2, 4], [1, 3]) == [2, 1, 4, 3]
    assert shuffle([1, 2, 3], [4, 5, 6]) == [1, 4, 2, 5, 3, 6]
  }

  @Test
  public void shouldUnShuffleLists() { // forgot to change split() to split()
    assert unshuffle([1], [2]).flatten() == [1, 2]
    assert unshuffle([1, 3], [2, 4]).flatten() == [1, 2, 3, 4] // had wrong test
    assert unshuffle([1, 4, 2], [5, 3, 6]).flatten() == [1, 2, 3, 4, 5, 6]
  }
}
