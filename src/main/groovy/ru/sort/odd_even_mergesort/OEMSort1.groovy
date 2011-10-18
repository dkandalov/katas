package ru.sort.odd_even_mergesort

import org.junit.Test

/**
 * User: dima
 * Date: 29/4/11
 */
class OEMSort1 {
  @Test public void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 1]) == [1, 1]
    assert sort([1, 1]) == [1, 1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([2, 1, 4, 3]) == [1, 2, 3, 4]
    assert sort([2, 1, 5, 4, 3]) == [1, 2, 3, 4, 5]
    assert sort([2, 1, 5, 4, 6, 3]) == [1, 2, 3, 4, 5, 6]
    assert sort([1, 2, 4, 3, 7, 8, 6, 5]) == [1, 2, 3, 4, 5, 6, 7, 8]

    [1, 2, 3, 4, 5, 6].permutations().each {
      assert sort(it) == [1, 2, 3, 4, 5, 6]
    }
  }

  def sort(List list) {
    if (list.size() < 2) return list

    int newSize = nextPowerOfTwo(list.size())
    def sizeDifference = newSize - list.size()
    sizeDifference.times { list << Integer.MAX_VALUE }

    def result = merge(sort(list[0..list.size() / 2 - 1]), sort(list[list.size() / 2..-1]))

    sizeDifference.times { result.remove(result.size() - 1) }
    result
  }

  @Test public void shouldFindNextPowerOfTwo() {
    def actual = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].collect { nextPowerOfTwo(it) }
    assert actual == [2, 2, 4, 4, 8, 8, 8, 8, 16, 16]
  }

  int nextPowerOfTwo(int n) {
    int i = 2
    while (i < n) i = i * 2
    i
  }

  @Test public void shouldMerge() {
    assert merge([1], [2]) == [1, 2]
    assert merge([1, 3], [2, 4]) == [1, 2, 3, 4]
    assert merge([1, 3, 6, 7], [2, 4, 5, 8]) == [1, 2, 3, 4, 5, 6, 7, 8] // used list of length 6 (not power of 2)
  }

  def merge(list1, list2) {
    if (list1.empty && list2.empty) return []
    if (list1.size() == 1 && list2.size() == 1) {
      return (list1[0] > list2[0]) ? list2 + list1 : list1 + list2
    }
    def list = list1 + list2
    list = unshuffle(list)

    def midPos = list.size() / 2
    list1 = merge(list[0..midPos / 2 - 1], list[midPos / 2..midPos - 1])
    list2 = merge(list[midPos..midPos + midPos / 2 - 1], list[midPos + midPos / 2..-1])

    list = shuffle(list1 + list2)

    1.step(list.size() - 1, 2) { // forgot to add this part of algorithm
      if (list[it] > list[it + 1]) {
        def tmp = list[it]
        list[it] = list[it + 1]
        list[it + 1] = tmp
      }
    }
    list
  }

  @Test public void shouldShuffleList() {
    assert shuffle([1, 2]) == [1, 2]
    assert shuffle([1, 2, 3, 4]) == [1, 3, 2, 4]
    assert shuffle([1, 2, 3, 4, 5, 6, 7, 8]) == [1, 5, 2, 6, 3, 7, 4, 8]
  }

  def shuffle(List list) {
    def result = []
    def listMid = list.size().intdiv(2) // used "/" instead of "intdiv()"
    0.upto(listMid - 1) { result << list[it] << list[listMid + it] }
    result
  }

  @Test public void shouldUnshuffleList() {
    assert unshuffle([1, 2]) == [1, 2]
    assert unshuffle([1, 2, 3, 4]) == [1, 3, 2, 4]
    assert unshuffle([1, 2, 3, 4, 5, 6, 7, 8]) == [1, 3, 5, 7, 2, 4, 6, 8]
    assert unshuffle([1, 5, 2, 6, 3, 7, 4, 8]) == [1, 2, 3, 4, 5, 6, 7, 8]
  }

  def unshuffle(List list) {
    def result = []
    0.step(list.size(), 2) { result << list[it] }
    1.step(list.size(), 2) { result << list[it] }
    result
  }
}
