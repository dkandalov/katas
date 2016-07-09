package hackerrank

import java.util.Collections.reverseOrder
import java.util.{Comparator, Scanner}

import org.junit.Test
import org.scalatest.Matchers

import scala.annotation.tailrec


class QueriesWithFixedLength extends Matchers {
	@Test def `example`(): Unit = {
		val result = processQueries(Seq(1, 2, 3, 4, 5), Seq(1, 2, 3, 4, 5))
		result should equal(Seq(1, 2, 3, 4, 5))
	}

	@Test def `adding elements to queue`(): Unit = {
		val q = new MinMaxQueue(3)
		q.add(1).toSeq should equal(Seq(1));       (q.min, q.max) should equal(1, 1)
		q.add(2).toSeq should equal(Seq(1, 2));    (q.min, q.max) should equal(1, 2)
		q.add(3).toSeq should equal(Seq(1, 2, 3)); (q.min, q.max) should equal(1, 3)
		q.add(4).toSeq should equal(Seq(2, 3, 4)); (q.min, q.max) should equal(2, 4)
		q.add(5).toSeq should equal(Seq(3, 4, 5)); (q.min, q.max) should equal(3, 5)
	}

	@Test def `add elements to heap`(): Unit = {
		val heap = new Heap(8, Ordering.Int)
		heap.size should equal(0)
		heap.data should equal(Seq(0, 0, 0, 0, 0, 0, 0, 0))

		heap.add(1)
		heap.size should equal(1)
		heap.data should equal(Seq(1, 0, 0, 0, 0, 0, 0, 0))

		heap.add(2)
		heap.size should equal(2)
		heap.data should equal(Seq(2, 1, 0, 0, 0, 0, 0, 0))

		heap.add(3)
		heap.size should equal(3)
		heap.data should equal(Seq(3, 1, 2, 0, 0, 0, 0, 0))

		heap.add(4)
		heap.size should equal(4)
		heap.data should equal(Seq(4, 3, 2, 1, 0, 0, 0, 0))

		heap.add(5)
		heap.size should equal(5)
		heap.data should equal(Seq(5, 4, 2, 1, 3, 0, 0, 0))
	}

	@Test def `remove elements from heap`(): Unit = {
		val heap = new Heap(8, Ordering.Int)
		List(1, 2, 3, 4, 5).foreach(heap.add)
		heap.size should equal(5)
		heap.data should equal(Seq(5, 4, 2, 1, 3, 0, 0, 0))

		heap.remove(1)
		heap.size should equal(4)
		heap.data should equal(Seq(5, 4, 2, 3, 0, 0, 0, 0))

		heap.remove(2)
		heap.size should equal(3)
		heap.data should equal(Seq(5, 4, 3, 0, 0, 0, 0, 0))

		heap.remove(3)
		heap.size should equal(2)
		heap.data should equal(Seq(5, 4, 0, 0, 0, 0, 0, 0))

		heap.remove(4)
		heap.size should equal(1)
		heap.data should equal(Seq(5, 0, 0, 0, 0, 0, 0, 0))

		heap.remove(5)
		heap.size should equal(0)
		heap.data should equal(Seq(0, 0, 0, 0, 0, 0, 0, 0))
	}

	def main(args: Array[String]): Unit = {
		val scanner = new Scanner(System.in)
		val n = scanner.nextInt()
		val q = scanner.nextInt()
		val seq = 0.until(n).map{ _ => scanner.nextInt() }
		val queries = 0.until(q).map{ _ => scanner.nextInt() }

		processQueries(seq, queries).foreach(println(_))
	}

	private def processQueries(seq: Seq[Int], queries: Seq[Int]): Seq[Int] = {
		queries.map{ d =>
			var from = 0
			var to = d
			var min = Int.MaxValue
			while (to <= seq.size) {
				var i = from
				var max = Int.MinValue
				while (i < to) {
					if (seq(i) > max) max = seq(i)
					i += 1
				}

				if (max < min) min = max

				from += 1
				to += 1
			}
			min
		}
	}

	/**
		* See also http://www.cs.otago.ac.nz/staffpriv/mike/Papers/MinMaxHeaps/MinMaxHeaps.pdf
		*/
	private class MinMaxQueue(maxSize: Int) {
		private val data: Array[Int] = Array.fill(maxSize)(Int.MinValue)
		private val minHeap: Heap = new Heap(maxSize, reverseOrder(Ordering.Int))
		private val maxHeap: Heap = new Heap(maxSize, Ordering.Int)
		private var index: Int = 0
		private var size: Int = 0

		def add(value: Int): MinMaxQueue = {
			if (size == maxSize) {
				val oldValue = data(index)
				minHeap.remove(oldValue)
				maxHeap.remove(oldValue)
			}
			data(index) = value
			index = (index + 1) % maxSize
			size = Math.min(size + 1, maxSize)

			minHeap.add(value)
			maxHeap.add(value)
			this
		}

		def min: Int = minHeap.top

		def max: Int = maxHeap.top

		def toSeq: Seq[Int] = {
			var result = Seq[Int]()
			var i = 0
			while (i < size) {
				result = data((index - 1 + maxSize - i) % maxSize) +: result
				i += 1
			}
			result
		}
	}


	private class Heap(maxSize: Int, comparator: Comparator[Int]) {
		private val _data: Array[Int] = new Array(maxSize)
		private var _size: Int = 0

		def add(value: Int): Unit = {
			_data(_size) = value
			bubbleUp(_size)
			_size += 1
		}

		def remove(value: Int, i: Int = 0): Boolean = {
			if (i >= _size) return false
			if (comparator.compare(value, _data(i)) > 0) return false
			if (comparator.compare(value, _data(i)) < 0) {
				val wasRemoved = remove(value, childOf(i))
				return if (!wasRemoved) remove(value, childOf(i) + 1) else wasRemoved
			}
			_data(i) = _data(_size - 1)
			_data(_size - 1) = 0
			_size -= 1
			if (comparator.compare(_data(i), _data(parentOf(i))) > 0) {
				bubbleUp(i)
			} else {
				sink(i)
			}
			true
		}

		def top: Int = _data(0)

		def size: Int = _size

		def data: Seq[Int] = _data.toSeq

		@tailrec private def sink(i: Int): Unit = {
			if (i >= _size) return
			var maxChild = if (comparator.compare(childOf(i), childOf(i) + 1) > 0) childOf(i) else childOf(i) + 1
			if (maxChild >= _size) maxChild = childOf(i)
			if (maxChild >= _size) return

			swap(_data, i, maxChild)
			sink(maxChild)
		}

		@tailrec private def bubbleUp(i: Int): Unit = {
			if (i > 0 && comparator.compare(_data(i), _data(parentOf(i))) > 0) {
				swap(_data, i, parentOf(i))
				bubbleUp(parentOf(i))
			}
		}

		private def swap(data: Array[Int], i1: Int, i2: Int) = {
			val tmp = data(i1)
			data(i1) = data(i2)
			data(i2) = tmp
		}

		private def parentOf(i: Int): Int = (i - 1) / 2

		private def childOf(i: Int): Int = (i * 2) + 1
	}
}