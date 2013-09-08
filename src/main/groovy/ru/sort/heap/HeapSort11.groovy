package ru.sort.heap

import org.junit.Test

class HeapSort11 {
  @Test void shouldSortListUsingHeap() {
    assert sort([]) == []

    assert sort([1]) == [1]

    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]

    (1..5).toList().permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5]
    }
  }

  private static Collection sort(List values) {
    new Heap().addAll(values).takeAll()
  }

  private static class Heap {
    private final data = []

    def addAll(Collection values) {
      values.each{ add(it) }
      this
    }

    def add(value) {
      data.add(value)
      swim(data.size() - 1)
      this
    }

    List takeAll() {
      def result = []
      while (!data.empty) result << take()
      result
    }

    def take() {
      def result = data[0]
      int index = sink(0)
      //noinspection GroovyAssignabilityCheck
      data.remove(index) // didn't remove
      result
    }

    private swim(int index) {
      int parent = parentIndexOf(index)
      if (data[index] < data[parent]) {
        exchange(index, parent)
        swim(parent)
      }
    }

    private int sink(int index) {
      int leftChild = leftChildOf(index)
      int rightChild = rightChildOf(index)
      if (leftChild >= data.size() && rightChild >= data.size()) return index

      int child
      if (leftChild < data.size() && rightChild >= data.size()) child = leftChild
      else child = data[leftChild] < data[rightChild] ? leftChild : rightChild

      exchange(index, child)
      sink(child)
    }

    private exchange(int index1, int index2) {
      def temp = data[index1]
      data[index1] = data[index2]
      data[index2] = temp
    }

    private static int parentIndexOf(int index) {
      (index + 1) / 2 - 1
    }

    private static int leftChildOf(int index) {
      index * 2 + 1
    }

    private static int rightChildOf(int index) {
      index * 2 + 2
    }
  }
}
