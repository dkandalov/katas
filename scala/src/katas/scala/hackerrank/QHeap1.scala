package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ArrayBuffer

/**
	* https://www.hackerrank.com/challenges/qheap1
	*/
class QHeap1 extends Matchers {

	@Test def `hackerrank example`(): Unit = {
		val heap = new Heap[Int]()
		heap.add(4)
		heap.add(9)
		heap.min shouldBe 4
		heap.delete(4)
		heap.min shouldBe 9
	}

	@Test def `min element while adding and deleting 10 elements`(): Unit = {
		val heap = new Heap[Int]()
		(-5).until(5).reverse.foreach { n =>
			heap.add(n)
			heap.min shouldBe n
		}
		(-5).until(5).foreach { n =>
			heap.delete(n)
			if (heap.size > 0) {
				heap.min shouldBe n + 1
			}
		}
	}

	@Test def `min element after add`(): Unit = {
		val heap = new Heap[Int]()
		heap.add(1)
		heap.min shouldBe 1
		heap.add(2)
		heap.min shouldBe 1
		heap.add(0)
		heap.min shouldBe 0
	}

	@Test def `min element after delete`(): Unit = {
		val heap = new Heap[Int]()
		heap.add(1)
		heap.min shouldBe 1
		heap.add(0)
		heap.min shouldBe 0
		heap.delete(0)
		heap.min shouldBe 1
	}

	@Test def `find index of element`(): Unit = {
		val heap = new Heap[Int]()
		1.until(8).foreach(heap.add)
		1.until(8).map { n => (n, heap.indexOf(n)) } shouldBe Seq(
			(1, 0),
			(2, 1), (3, 2),
			(4, 3), (5, 4), (6, 5), (7, 6)
		)
		heap.indexOf(0) shouldBe -1
		heap.indexOf(10) shouldBe -1
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)
		val n = scanner.nextLine.toInt

		val heap = new Heap[Long]()
		0.until(n).foreach { _ =>
			val values = scanner.nextLine().split(" ").map(_.toInt)
			if (values(0) == 1) heap.add(values(1))
			else if (values(0) == 2) heap.delete(values(1))
			else if (values(0) == 3) println(heap.min)
		}
	}

	class Heap[T](implicit orderer: T => Ordered[T]) {
		private val data: ArrayBuffer[T] = ArrayBuffer()

		def min: T = data(0)

		def size: Int = data.size

		def add(n: T): Unit = {
			data.append(n)
			swim(data.size - 1)
		}

		def delete(n: T): Unit = {
			val i = indexOf(n)
			swap(i, data.size - 1, data)
			data.remove(data.size - 1)
			sink(i)
		}

		private def sink(index: Int): Unit = {
			val childIndex = minChildIndex(index)
			if (childIndex == -1) return
			if (data(index) <= data(childIndex)) return
			
			swap(index, childIndex, data)
			sink(childIndex)
		}

		private def minChildIndex(index: Int): Int = {
			val left = leftChildIndex(index)
			val right = rightChildIndex(index)

			if (left >= data.size) -1
			else if (right >= data.size) left
			else if (data(left) < data(right)) left
			else right
		}

		private def swim(index: Int): Unit = {
			var i = index
			var pi = parentIndex(i)
			while (i != pi && data(i) < data(pi)) {
				swap(i, pi, data)
				i = parentIndex(i)
				pi = parentIndex(pi)
			}
		}

		private[QHeap1] def indexOf(n: T, i: Int = 0): Int = {
			if (n < data(i)) return -1
			if (n == data(i)) return i

			var result = -1
			val ci1 = leftChildIndex(i)
			val ci2 = rightChildIndex(i)
			if (ci1 < data.size) {
				result = indexOf(n, ci1)
			}
			if (result == -1 && ci2 < data.size) {
				result = indexOf(n, ci2)
			}
			result
		}

		private def leftChildIndex(i: Int): Int = (i * 2) + 1

		private def rightChildIndex(i: Int): Int = (i * 2) + 2

		private def parentIndex(i: Int): Int = i / 2

		private def swap(i1: Int, i2: Int, data: ArrayBuffer[T]) = {
			val tmp = data(i1)
			data(i1) = data(i2)
			data(i2) = tmp
		}
	}
}