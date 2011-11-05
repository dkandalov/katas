package ru.sort.heap

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import java.lang.IllegalStateException
import ru.permutation.Perm3_
import ru.util.Mess

/**
 * User: dima
 * Date: 05/11/2011
 */
@Mess
class HeapSort4 extends ShouldMatchers {
  @Test def shouldSortList() {
    sort(List()) should equal(List())
    sort(List(1)) should equal(List(1))
    sort(List(1, 2)) should equal(List(1, 2))
    sort(List(2, 1)) should equal(List(1, 2))
    sort(List(3, 1, 2)) should equal(List(1, 2, 3))

    new Perm3_().perm(List(1, 2, 3, 4, 5)).foreach { list =>
      sort(list) should equal(List(1, 2, 3, 4, 5))
    }
  }

  def sort(list: List[Int]): List[Int] = {
    collectMaxValuesFrom(list.foldRight(Heap()) { (i, heap) => heap.add(i) })
  }

  private def collectMaxValuesFrom(heap: Heap): List[Int] = {
    if (heap == null || heap == Heap()) List()
    else collectMaxValuesFrom(heap.removeMax()._2) ::: List(heap.value)
  }

  @Test def shouldAddElementsToHeap() {
    Heap() should equal(Heap())
    Heap(1) should equal(Heap(1))

    Heap().add(4) should equal(Heap(4))
    Heap().add(4).add(3) should equal(Heap(4, Heap(3)))
    Heap().add(4).add(3).add(2) should equal(Heap(4, Heap(3), Heap(2)))
    Heap().add(4).add(2).add(3) should equal(Heap(4, Heap(2), Heap(3)))
    Heap().add(4).add(2).add(3).add(1) should equal(Heap(4, Heap(2, Heap(1)), Heap(3)))

    Heap().add(4).add(5) should equal(Heap(5, Heap(4)))
    Heap().add(4).add(5).add(3) should equal(Heap(5, Heap(4), Heap(3)))

    Heap().add(4).add(4) should equal(Heap(4, Heap(4)))
    Heap().add(4).add(4).add(4) should equal(Heap(4, Heap(4), Heap(4)))
  }

  @Test def shouldRemoveMaxElementFromHeap() {
    Heap().removeMax() should equal((Int.MinValue, null))
    Heap(3).removeMax() should equal((3, null))
    Heap(3, Heap(2)).removeMax() should equal((3, Heap(2)))
    Heap(3, Heap(2), Heap(1)).removeMax() should equal((3, Heap(2, null, Heap(1))))
    Heap(3, Heap(1), Heap(2)).removeMax() should equal((3, Heap(2, Heap(1))))
    Heap(3, Heap(1, Heap(0)), Heap(2)).removeMax() should equal((3, Heap(2, Heap(1, Heap(0)))))
  }

  case class Heap(value: Int = Int.MinValue, left: Heap = null, right: Heap = null) {
    def removeMax(): (Int, Heap) = {
      if (value == Int.MinValue) (value, null)
      else {
        (value, sink())
      }
    }

    private def sink(): Heap = {
      if (left == null && right == null) null
      else if (left != null && right == null) Heap(left.value, left.sink(), null)
      else if (right != null && left == null) Heap(right.value, null, right.sink()) // copy-pasted left.value
      else if (left.value > right.value) Heap(left.value, left.sink(), right)
      else Heap(right.value, left, right.sink()) // copy-pasted left.value
    }

    def add(newValue: Int): Heap = {
      if (value == Int.MinValue) {
        Heap(newValue)
      } else if (newValue <= value) {
        if (left == null) Heap(value, Heap(newValue), right)
        else if (right == null) Heap(value, left, Heap(newValue))
        else if (left.value < right.value) {
          Heap(value, left.add(newValue), right)
        } else {
          Heap(value, left, right.add(newValue))
        }
      } else if (newValue > value) {
        Heap(newValue, this)
      } else {
        throw new IllegalStateException()
      }
    }
  }
}