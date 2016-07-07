package hackerrank

import java.util.Scanner

import org.junit.Test
import org.scalatest.Matchers


class QueriesWithFixedLength extends Matchers {
	@Test def `example`(): Unit = {
		val result = processQueries(Seq(1, 2, 3, 4, 5), Seq(1, 2, 3, 4, 5))
		result should equal(Seq(1, 2, 3, 4, 5))
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
}