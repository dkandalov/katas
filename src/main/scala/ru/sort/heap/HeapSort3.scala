package ru.sort.heap

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
;

/*
 * User: dima
 * Date: 19/3/11
 * Time: 9:37 AM
 */
class HeapSort3 extends AssertionsForJUnit {
  @Test
  def shouldSortList() {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(1, 1)) === List(1, 1))
    assert(sort(List(1, 2)) === List(1, 2))
    assert(sort(List(2, 1)) === List(1, 2))
    assert(sort(List(2, 3, 1)) === List(1, 2, 3))
    assert(sort(List(2, 2, 3, 1)) === List(1, 2, 2, 3))
  }

  def sort(list: List[Int]): List[Int] = {
    val heap = new Heap()
    list.foreach(heap.add(_))

    var result: List[Int] = List()
    while (!heap.isEmpty) result = heap.takeMaxValue :: result
    result
  }
}

class Heap {
  val data = new Array[Int](10)
  var size = 0

  def isEmpty: Boolean = {
    size <= 0
  }

  def takeMaxValue(): Int = {
    val result = data(0)
    data(0) = data(size - 1)
    size -= 1
    sink(0)
    result
  }

  private def sink(index: Int) {
    if (index >= size - 1) return
    val maxChild = if (index * 2 + 2 >= size)
      if (data(index * 2 + 1) > data(index * 2 + 2)) (index * 2 + 1) else (index * 2 + 2)
    else
      (index * 2 + 1)

    if (data(index) < data(maxChild)) {
      swap(index, maxChild)
      sink(maxChild)
    }
  }

  private def swap(i1: Int, i2: Int) {
    val tmp = data(i1)
    data(i1) = data(i2)
    data(i2) = tmp
  }

  def add(value: Int) {
    size += 1
    data(size - 1) = value
    swim(size - 1)
  }

  private def swim(index: Int) {
    if (index == 0) return

    val parentIndex = index / 2
    if (data(index) > data(parentIndex)) {
      swap(index, parentIndex)
      swim(parentIndex)
    }
  }
}
