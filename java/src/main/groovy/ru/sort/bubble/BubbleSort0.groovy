package ru.sort.bubble

import org.junit.Test

 /**
 * User: dima
 * Date: 27/1/11
 */
class BubbleSort0 {
  @Test
  void bubbleSort() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([3, 2, 3, 1]) == [1, 2, 3, 3]
    assert sort([3, 2, 1, -1]) == [-1, 1, 2, 3]

    assert sort(["z", "f", "a"]) == ["a", "f", "z"]

    (100).times {
      assert sort(shuffledList(1..100)) == (1..100).asList()
    }
  }

  @Test public void shouldShuffle() {
    // should be not equal to unsorted list at least once
    assert (1..10).inject(false) { acc, value ->
      acc | (shuffledList(1..100) != (1..100))
    }
  }

  static def sort(List values) {
    for (int i = 0; i < values.size(); i++) {
      for (int j = values.size() - 1; j > i; j--) {
        if (values[j - 1] > values[j]) {
            swapWithPrevious(values, j)
        }
      }
    }
    values
  }

    private static def swapWithPrevious(List values, int j) {
        def tmp = values[j - 1]
        values[j - 1] = values[j]
        values[j] = tmp
    }

  static List shuffledList(def range) {
    def list = new LinkedList((range).asList())
    Collections.shuffle(list)
    list
  }
}
