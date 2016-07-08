package hackerrank

import java.util.Scanner

import org.junit.Test
import org.scalatest.Matchers


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

	private class MinMaxQueue(maxSize: Int) {
		// TODO wrong, need to use heap
		private val data: Array[Int] = new Array(maxSize)
		private val minData: Array[Int] = Array.fill(maxSize)(Int.MaxValue)
		private val maxData: Array[Int] = Array.fill(maxSize)(Int.MinValue)
		private var minValue: Int = Int.MaxValue
		private var maxValue: Int = Int.MinValue
		private var from: Int = 0
		private var to: Int = 0
		private var size: Int = 0

		def add(value: Int): MinMaxQueue = {
			val oldestValue = data(to)

			data(to) = value

			if (size == maxSize) {
				if (oldestValue == minData(to)) {
				}
				if (oldestValue == maxValue) {
				}
			}
			if (value < minData(to)) {
				minData(to) = value
			}
			if (value > maxData(to)) {
				maxData(to) = value
			}

			to = (to + 1) % maxSize
			if (size < maxSize) {
				size += 1
			} else {
				from = (from + 1) % maxSize
			}
			this
		}

		def min: Int = minValue

		def max: Int = maxValue

		def toSeq: Seq[Int] = {
			var result = Seq[Int]()
			var i = from
			var count = size
			while (i != to || count > 0) {
				result = result :+ data(i)
				i = (i + 1) % data.length
				count -= 1
			}
			result
		}
	}
}