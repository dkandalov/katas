package katas.scala.sort.heap

import org.scalatest.Matchers
import org.junit.Test
import collection.Seq
import java.lang.IllegalStateException

/**
 * User: dima
 * Date: 30/01/2012
 */
class HeapSort7 extends Matchers {
  @Test def GIVEN_aSequence_SHOULD_sortUsingHeapSort() {
    new Heap().addAll(Seq()).takeAll() should equal(Seq())
    new Heap().addAll(Seq(1)).takeAll() should equal(Seq(1))
    new Heap().addAll(Seq(1, 2)).takeAll() should equal(Seq(1, 2))
    new Heap().addAll(Seq(2, 1)).takeAll() should equal(Seq(1, 2))
    new Heap().addAll(Seq(2, 3, 1)).takeAll() should equal(Seq(1, 2, 3))
    new Heap().addAll(Seq(2, 4, 3, 1)).takeAll() should equal(Seq(1, 2, 3, 4))
    List(1, 2, 3, 4, 5).permutations.toList.foreach { list =>
        new Heap().addAll(list).takeAll() should equal(Seq(1, 2, 3, 4, 5))
    }
  }

  private class Heap() {
    val data = new Array[Int](10)
    var size: Int = 0

    def addAll(seq: Seq[Int]): Heap = {
      if (seq.isEmpty) return this
      add(seq.head).addAll(seq.tail)
    }

    def add(value: Int): Heap = {
      if (size >= data.length) throw new IllegalStateException()
      data(size) = value
      size += 1
      swim(size - 1)
      this
    }

    def takeAll(): Seq[Int] = {
      if (size == 0) return Seq() // didn't have this
      val result = takeTop()
      result._2.takeAll().+:(result._1)
    }

    def takeTop(): (Int, Heap) = {
      size -= 1
      swap(0, size)
      val result = data(size)
      sink(0)
      (result, this)
    }

    private def swim(i: Int) {
      if (i > 0 && data(parentOf(i)) > data(i)) {
        swap(parentOf(i), i)
        swim(parentOf(i))
      }
    }

    private def sink(i: Int) {
      def findMinChild: Int = {
        if (leftChildOf(i) >= size) -1
        else if (rightChildOf(i) >= size) leftChildOf(i) // didn't have this
        else if (data(leftChildOf(i)) < data(rightChildOf(i))) leftChildOf(i) else rightChildOf(i)
      }
      val minChild = findMinChild
      if (minChild != -1 && data(i) > data(minChild)) {
        swap(minChild, i)
        sink(minChild)
      }
    }

    def leftChildOf(i: Int) = i * 2 + 1

    def rightChildOf(i: Int) = i * 2 + 2

    def parentOf(i: Int) = (i - 1) / 2

    private def swap(i1: Int, i2: Int) {
      val tmp = data(i2)
      data(i2) = data(i1)
      data(i1) = tmp
    }

  }
}