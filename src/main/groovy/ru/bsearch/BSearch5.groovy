package ru.bsearch

import org.junit.Test

/**
 * @author DKandalov
 */
class BSearch5 {
  @Test
  void shouldFindIndexOfElementInAList() {
    assert search(1, []) == -1

    assert search(0, [1]) == -1
    assert search(1, [1]) == 0
    assert search(2, [1]) == -1

    assert search(0, [1, 2]) == -1
    assert search(1, [1, 2]) == 0
    assert search(2, [1, 2]) == 1
    assert search(3, [1, 2]) == -1

    assert search(0, [1, 2, 3]) == -1
    assert search(1, [1, 2, 3]) == 0
    assert search(2, [1, 2, 3]) == 1
    assert search(3, [1, 2, 3]) == 2
    assert search(4, [1, 2, 3]) == -1

    (1..1000).toList().with { list ->
      list.each { i -> assert search(i, list) == i - 1 }
      assert search(0, list) == -1
      assert search(list.size() + 1, list) == -1
    }
  }

  int search(def elem, List list, int shift = 0) {
    if (list.empty) return -1

    int mid = list.size().intdiv(2)
    if (elem == list[mid]) {
      shift + mid
    } else if (elem < list[mid]) {
      search(elem, list.subList(0, mid), shift)
    } else if (elem > list[mid]) {
      search(elem, list.subList(mid + 1, list.size()), shift + mid + 1) // had "shift + mid"
    } else {
      throw new IllegalStateException("List must be sorted")
    }
  }
}
