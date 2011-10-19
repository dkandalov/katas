package ru.bsearch

import org.junit.Test

/**
 * @author DKandalov
 */
class BSearch4_ {
  @Test
  public void shouldFindIndexOfElementInAList() {
    assert bSearch(1, []) == -1

    assert bSearch(0, [1]) == -1
    assert bSearch(1, [1]) == 0
    assert bSearch(2, [1]) == -1

    assert [0, 1, 2, 3].collect { bSearch(it, [1, 2])} == [-1, 0, 1, -1] // had mistake in test
    assert [0, 1, 2, 3, 4].collect { bSearch(it, [1, 2, 3])} == [-1, 0, 1, 2, -1]
    assert [0, 1, 2, 3, 4, 5].collect { bSearch(it, [1, 2, 3, 4])} == [-1, 0, 1, 2, 3, -1]
  }

  static bSearch(value, list) {
    int from = 0
    int to = list.size

    while (to - from > 0) { // had > 1 bad corrected before running
      int midPos = (from + to) / 2
      if (value == list[midPos]) {
        return midPos
      } else if (value < list[midPos]) {
        to = midPos
      } else if (value > list[midPos]) {
        from = midPos + 1
      }
    }
    return -1
  }
}
