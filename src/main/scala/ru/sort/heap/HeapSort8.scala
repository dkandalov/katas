package ru.sort.heap

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import java.lang.IllegalStateException
import annotation.tailrec

/**
 * User: dima
 * Date: 06/02/2012
 */
class HeapSort8 extends ShouldMatchers {
  @Test def GIVEN_aSequence_SHOULD_sortItUsingHeapSort() {
    sort(Seq()) should equal(Seq())
    sort(Seq(1)) should equal(Seq(1))
    sort(Seq(1, 2)) should equal(Seq(1, 2))
    sort(Seq(2, 1)) should equal(Seq(1, 2))
    sort(Seq(2, 3, 1)) should equal(Seq(1, 2, 3))
    Seq(1, 2, 3, 4, 5).permutations.foreach { seq =>
      sort(seq) should equal(Seq(1, 2, 3, 4, 5))
    }
  }

  def sort(seq: Seq[Int]): Seq[Int] = {
    new Heap().addAll(seq).takeAll()._1
  }

  private class Heap {
    val data: Array[Int] = new Array[Int](10)
    var size: Int = 0

    @tailrec final def addAll(seq: Seq[Int]): Heap = {
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

    @tailrec final def takeAll(seq: Seq[Int] = Seq()): (Seq[Int], Heap) = {
      if (size == 0) return (seq, this)
      val result = takeTop()
      result._2.takeAll(seq.:+(result._1))
    }

    def takeTop(): (Int, Heap) = {
      val topValue = data(0)

      size -= 1
      swap(0, size)
      sink(0)

      (topValue, this)
    }

    @tailrec private def sink(i: Int) {
      val minChild = minChildOf(i)
      if (minChild._1 == -1) return

      if (data(i) > minChild._2) {
        swap(i, minChild._1)
        sink(minChild._1)
      }
    }

    private def minChildOf(i: Int): (Int, Int) = {
      val leftChild = leftChildOf(i)
      val rightChild = rightChildOf(i)

      if (leftChild >= size) (-1, -1)
      else if (rightChild >= size) (leftChild, data(leftChild))
      else if (data(leftChild) < data(rightChild)) (leftChild, data(leftChild))
      else (rightChild, data(rightChild))
    }

    @tailrec private def swim(i: Int) {
      if (i == 0) return

      val parent = parentOf(i)
      if (data(i) < data(parent)) {
        swap(i, parent)
        swim(parent)
      }
    }

    private def swap(i1: Int, i2: Int) {
      val tmp = data(i1)
      data(i1) = data(i2)
      data(i2) = tmp
    }

    private def leftChildOf(i: Int): Int = i * 2 + 1

    private def rightChildOf(i: Int): Int = i * 2 + 2

    private def parentOf(i: Int): Int = (i - 1) / 2

  }
}