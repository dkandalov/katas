package katas.groovy.sort.insertsort

import org.junit.Test
import katas.groovy.sort.bubble.BubbleSort0

/**
 * User: dima
 * Date: 29/1/11
 */
class InsertionSort0 {
  @Test
  public void shouldSort() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([3, 2, 3, 1]) == [1, 2, 3, 3]
    assert sort([3, 2, 1, -1]) == [-1, 1, 2, 3]

    assert sort(["z", "f", "a"]) == ["a", "f", "z"]

    (100).times {
      assert sort(BubbleSort0.shuffledList(1..100)) == (1..100).asList()
    }
  }

  static def sort(List list) {
    for (int i = 1; i < list.size(); i++) {
      for (int j = i; j > 0; j--) {
        if (list[j - 1] <= list[j]) break

        def tmp = list[j]
        list[j] = list[j - 1]
        list[j - 1] = tmp
      }
    }
    list
  }
}
