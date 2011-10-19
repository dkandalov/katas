package ru.proprityQueue

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import collection.mutable.ListBuffer
import ru.permutation.{Perm1_, Perm1}

// Ugly and very procedural
class QueueSort0 extends AssertionsForJUnit {
  @Test
  def shouldSortListUsingPriorityQueue() {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(1, 2)) === List(1, 2))
    assert(sort(List(2, 1)) === List(1, 2))
    assert(sort(List(2, 3, 1)) === List(1, 2, 3))
    assert(sort(List(4, 2, 3, 1)) === List(1, 2, 3, 4))
    assert(sort(List(4, 2, 5, 3, 1)) === List(1, 2, 3, 4, 5))

    new Perm1_().permutation(ListBuffer(1, 2, 3, 4, 5)).foreach {
      listBuffer => assert(sort(listBuffer.toList) == List(1, 2, 3, 4, 5))
    }
  }

  def sort(list: List[Int]): List[Int] = {
    val queue = new Queue()
    list.foreach {
      queue.add(_)
    }

    var result = List[Int]()
    while (!queue.isEmpty) {
      //      println(Arrays.toString(queue.data) + " - " + queue.size)
      result = queue.take() :: result
    }
    result
  }

  private class Queue {
    val data = new Array[Int](10)
    var size = 0

    def add(value: Int) {
      data(size) = value
      size = size + 1
      swim(size - 1)
    }

    def take(): Int = {
      val result = data(0)
      data(0) = data(size - 1)
      size = size - 1
      sink(0)
      result
    }

    private def swim(index: Int) {
      var i = index

      while (i != 0) {
        val parentIndex = (i - 1) / 2
        val parentValue = data(parentIndex)
        if (data(i) > parentValue) {
          val tmp = data(i)
          data(i) = data(parentIndex)
          data(parentIndex) = tmp

          i = parentIndex
        } else {
          return
        }
      }
    }

    private def sink(index: Int) {
      var i = index

      var child1Index = (i * 2) + 1
      var child2Index = (i * 2) + 2

      // had wrong condition checking if both child1Index and child2Index are less than size
      while (child1Index < size) {
        val maxChildIndex = if (child2Index >= size) {
          child1Index
        } else {
          if (data(child1Index) > data(child2Index)) child1Index else child2Index // was named minChildIndex when it should be maxChildIndex
        }
        //        println(i)

        if (data(i) < data(maxChildIndex)) {
          val tmp = data(i)
          data(i) = data(maxChildIndex)
          data(maxChildIndex) = tmp

          i = maxChildIndex
          child1Index = (i * 2) + 1 // didn't recalculate child indexes
          child2Index = (i * 2) + 2
        } else {
          return
        }
      }
    }

    def isEmpty() = size == 0
  }

}

