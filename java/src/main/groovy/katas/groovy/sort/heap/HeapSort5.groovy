package katas.groovy.sort.heap

import org.junit.Test
import static katas.groovy.sort.heap.HeapSort5.Heap.heap

/**
 * User: dima
 * Date: 07/01/2012
 */
class HeapSort5 {

  @Test public void shouldReturnContentsOfHeapInSortedOrder() {
    assert heap([3, 2, 1]).takeAll() == [3, 2, 1]
    assert heap([1, 2, 3]).takeAll() == [3, 2, 1]
    assert heap([2, 1, 3]).takeAll() == [3, 2, 1]
    [1, 2, 3, 4, 5].each { size ->
      (1..size).toList().permutations().each { list ->
        assert heap(list).takeAll() == (1..size).toList().reverse()
      }
    }
  }

  @Test public void shouldKeepHeapStructureWhenAddingValues() {
    assert heap().toString() == "()"
    assert heap().add(1).toString() == "(1)"
    assert heap().add(1).add(2).toString() == "(2(1,))"
    assert heap().add(2).add(1).toString() == "(2(1,))"
    assert heap().add(1).add(2).add(3).toString() == "(3(1,2))"
    assert heap().add(3).add(1).add(2).toString() == "(3(1,2))"
    assert heap().add(3).add(2).add(1).toString() == "(3(2,1))"
    assert heap().add(1).add(1).add(1).toString() == "(1(1,1))"
  }

  @Test public void shouldKeepHeapStructureWhenRemovingMaxValue() {
    heap([1, 2, 3]).with {
      assert it.toString() == "(3(1,2))"
      assert it.takeMax() == 3
      assert it.toString() == "(2(1,))"
      assert it.takeMax() == 2
      assert it.toString() == "(1)"
      assert it.takeMax() == 1
      assert it.toString() == "()"
    }
  }

  static class Heap {
    static Heap heap(values = []) {
      values.inject(new Heap()) { heap, value ->
        heap.add(value)
      }
    }

    def data = []

    def takeAll() {
      def result = []
      while (!data.isEmpty()) result << takeMax()
      result
    }

    def takeMax() {
      exchange(0, data.size() - 1)
      def result = data.remove(data.size() - 1)
      sink(0)
      result
    }

    Heap add(def value) {
      data.add(value)
      swim(data.size() - 1)
      this
    }

    def sink(int index) {
      def maxChild
      if (rightChild(index) < data.size() && data[leftChild(index)] <= data[rightChild(index)]) {
        maxChild = rightChild(index)
      } else {
        maxChild = leftChild(index)
      }
      if (data[index] < data[maxChild]) {
        exchange(index, maxChild)
        sink(maxChild)
      }
    }

    def swim(int index) {
      int parentIndex = parent(index)
      if (parentIndex >= 0 && data[index] > data[parentIndex]) {
        exchange(index, parentIndex)
        swim(parentIndex)
      }
    }

    def exchange(int i1, int i2) {
      def tmp = data[i1]
      data[i1] = data[i2]
      data[i2] = tmp
    }

    int parent(int index) {
      (index + 1).intdiv(2) - 1
    }

    int leftChild(index) {
      (index + 1) * 2 - 1
      // this is better (the code above is stupid)
      // index * 2 + 1
    }

    int rightChild(index) {
      (index + 1) * 2
      // this is better
      // index * 2 + 2
    }

    @Override
    String toString() {
      "(" + toString(0) + ")"
    }

    def toString(index) {
      if (index >= data.size()) {
        ""
      } else {
        def leftChildren = toString(leftChild(index))
        def rightChildren = toString(rightChild(index))
        if (!leftChildren.isEmpty() || !rightChildren.isEmpty()) {
          "${data[index]}($leftChildren,$rightChildren)"
        } else {
          "${data[index]}"
        }
      }
    }
  }
}
