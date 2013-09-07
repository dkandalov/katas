package ru.sort.heap

import org.junit.Test

class HeapSort9 {
  @Test void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]

    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]

    assert sort([1, 2, 3]) == [1, 2, 3]
    assert sort([1, 3, 2]) == [1, 2, 3]
    assert sort([2, 1, 3]) == [1, 2, 3]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([3, 1, 2]) == [1, 2, 3]
    assert sort([3, 2, 1]) == [1, 2, 3]

    (1..5).toList().permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5]
    }
  }

  static sort(List values) {
    def heap = new Heap(values)
    def result = []
    while (!heap.empty) result << heap.takeTop()
    result
  }

  private static class Heap {
    private final data = []

    Heap(List values) {
      values.each{ add(it) }
    }

    def add(value) {
      data << value
      swim(data.size() - 1)
      this
    }

    def takeTop() {
      def result = data[0]
      int index = sink(0)
      //noinspection GroovyAssignabilityCheck
      data.remove(index)
      result
    }

    boolean isEmpty() {
      data.isEmpty()
    }

    private swim(int index) {
      int parentIndex = parentIndexOf(index)
      if (data[index] < data[parentIndex]) {
        exchange(index, parentIndex)
        swim(parentIndex)
      }
    }

    private int sink(int index) { // didn't return sunk index
      int leftChild = leftChildOf(index)
      int rightChild = rightChildOf(index)

      if (leftChild >= data.size() && rightChild >= data.size()) return index // compared index with 0

      int childIndex
      if (leftChild < data.size() && rightChild >= data.size()) {
        childIndex = leftChild
      } else {
        childIndex = data[leftChild] < data[rightChild] ? leftChild : rightChild
      }
      exchange(index, childIndex)
      sink(childIndex)
    }

    private static int leftChildOf(int index) {
      index * 2 + 1
    }

    private static int rightChildOf(int index) {
      index * 2 + 2
    }

    private static int parentIndexOf(int index) {
      (index + 1) / 2 - 1
    }

    private exchange(int index1, int index2) {
      def temp = data[index1]
      data[index1] = data[index2]
      data[index2] = temp
    }
  }
}
