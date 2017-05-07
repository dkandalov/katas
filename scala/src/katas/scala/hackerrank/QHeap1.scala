package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
	* https://www.hackerrank.com/challenges/qheap1
	*/
class QHeap1 extends Matchers {

	@Test def `hackerrank example`(): Unit = {
		val heap = new Heap[Int]()
		heap.add(4)
		heap.add(9)
		heap.min shouldEqual 4
		heap.delete(4) shouldEqual true
		heap.min shouldEqual 9
	}

	@Test def `min element while adding and deleting 10 elements`(): Unit = {
		val heap = new Heap[Int]()
		(-5).until(5).reverse.foreach { n =>
			heap.add(n)
			heap.min shouldEqual n
		}
		(-5).until(5).foreach { n =>
			heap.delete(n) shouldEqual true
			if (heap.size > 0) {
				heap.min shouldEqual n + 1
			}
		}
	}

	@Test def `min element after add`(): Unit = {
		val heap = new Heap[Int]()
		heap.add(1)
		heap.min shouldEqual 1
		heap.add(2)
		heap.min shouldEqual 1
		heap.add(0)
		heap.min shouldEqual 0
	}

	@Test def `min element after delete`(): Unit = {
		val heap = new Heap[Int]()
		heap.add(1)
		heap.min shouldEqual 1
		heap.add(0)
		heap.min shouldEqual 0
		heap.delete(0) shouldEqual true
		heap.min shouldEqual 1
	}

	@Test def `find index of element`(): Unit = {
		val heap = new Heap[Int]()
		1.until(8).foreach(heap.add)
		1.until(8).map { n => (n, heap.indexOf(n)) } shouldEqual Seq(
			(1, 0),
			(2, 1), (3, 2),
			(4, 3), (5, 4), (6, 5), (7, 6)
		)
		heap.indexOf(0) shouldEqual -1
		heap.indexOf(10) shouldEqual -1
	}

	@Test def `example which fails when parent index lookup is incorrect`(): Unit = {
		val values = Seq(3, 5, 1, 2, 4, 6, 0)

		val heap = new Heap[Int]()
		values.foreach { n =>
			heap.add(n)
			heap.toList.foreach { it =>
				(it, heap.indexOf(it)) shouldNot equal(it, -1)
			}
		}
	}

	@Test def `example which fails when delete doesn't do swim/sink`(): Unit = {
		val values = Seq(62, 65, -90, 82, -3, 22, 81, -56, -51, -49)
		val heap = new Heap[Long]()

		values.foreach { n => heap.add(n) }
		
		values.foreach { n =>
			heap.delete(n) shouldEqual true
			heap.toList.foreach { it =>
				(it, heap.indexOf(it)) shouldNot equal(it, -1)
			}
		}
	}

	@Test def `add and remove random elements`(): Unit = {
		val seed = new Random().nextInt
		println(s"seed = $seed")
		val random = new Random(seed)

		val heap = new Heap[Long]()
		val values = 0.until(1000).map { _ => random.nextLong() }.distinct

		values.foreach { n => heap.add(n) }
		values.foreach { n =>
			heap.delete(n) shouldEqual true
		}
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

	class Heap[T](private val data: ArrayBuffer[T] = ArrayBuffer[T]())(implicit orderer: T => Ordered[T]) {

		def min: T = data(0)

		def size: Int = data.size

		def add(n: T): Unit = {
			data.append(n)
			swim(data.size - 1)
		}

		def delete(n: T): Boolean = {
			val i = indexOf(n)
			if (i == -1) return false

			swap(i, data.size - 1, data)
			data.remove(data.size - 1)
			if (i < data.size) {
				swim(i)
				sink(i)
			}
			true
		}

		def toList: List[T] = data.toList

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

		private def swim(index: Int) {
			var i = index
			var pi = parentIndexOf(i)
			while (i != pi && data(i) < data(pi)) {
				swap(i, pi, data)
				i = parentIndexOf(i)
				pi = parentIndexOf(pi)
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

		private def leftChildIndex(i: Int) = (i * 2) + 1

		private def rightChildIndex(i: Int) = (i * 2) + 2

		private def parentIndexOf(i: Int) = (i - 1) / 2

		private def swap(i1: Int, i2: Int, data: ArrayBuffer[T]) {
			val tmp = data(i1)
			data(i1) = data(i2)
			data(i2) = tmp
		}
	}
}