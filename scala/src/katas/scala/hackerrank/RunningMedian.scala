package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ArrayBuffer

/**
	* https://www.hackerrank.com/challenges/find-the-running-median
	*/
class RunningMedian extends Matchers {

	@Test def `hackerrank example`(): Unit = {
		val runningMedian = new RunningMedian()
		Seq(12, 4, 5, 3, 8, 7).map{ runningMedian.add } shouldBe Seq(12.0, 8.0, 5.0, 4.5, 5.0, 6.0)
	}

	@Test def `large amount of items`(): Unit = {
		val runningMedian = new RunningMedian()
		Range(0, 100000).map{ runningMedian.add }
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)
		var n = scanner.nextLine.toInt

		val runningMedian = new RunningMedian()
		while (n > 0) {
			println(runningMedian.add(scanner.nextLine.toInt))
			n -= 1
		}
	}

	class RunningMedian {
		private val maxHeap = Heap[Int](){ it => math.Ordered.orderingToOrdered(it)(Ordering[Int].reverse) }
		private val minHeap = Heap[Int]()

		def add(n: Int): Double = {
			if (maxHeap.size > 0 && n < maxHeap.data(0)) {
				maxHeap.add(n)
			} else {
				minHeap.add(n)
			}
			while ((maxHeap.size - minHeap.size) > 1) {
				minHeap.add(maxHeap.delete(0))
			}
			while ((minHeap.size - maxHeap.size) > 1) {
				maxHeap.add(minHeap.delete(0))
			}
			if (maxHeap.size == minHeap.size) {
				(maxHeap.data(0) + minHeap.data(0)) / 2.0
			} else if (maxHeap.size > minHeap.size) {
				maxHeap.data(0)
			} else {
				minHeap.data(0)
			}
		}
	}


	case class Heap[T](data: ArrayBuffer[T] = ArrayBuffer[T]())(implicit ordered: T => Ordered[T]) {

		def size: Int = data.size

		def add(n: T): Unit = {
			data.append(n)
			swim(data.size - 1)
		}

		def delete(index: Int): T = {
			val result = data(0)
			swap(index, data.size - 1, data)
			data.remove(data.size - 1)
			if (index < data.size) {
				swim(index)
				sink(index)
			}
			result
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

		private def swim(index: Int) {
			var i = index
			var pi = parentIndexOf(i)
			while (i != pi && data(i) < data(pi)) {
				swap(i, pi, data)
				i = parentIndexOf(i)
				pi = parentIndexOf(pi)
			}
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