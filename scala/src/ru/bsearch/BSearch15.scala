package ru.bsearch

import org.scalatest.Matchers
import org.junit.Test

/**
 * User: dima
 * Date: 15/09/2012
 */

class BSearch15 extends Matchers {
	@Test def findIndexOfElementInASequence() {
		given(Seq()) {
			bsearch(1, _) should equal(-1)
		}
		given(Seq(1)) { seq =>
			bsearch(Int.MinValue, seq) should equal(-1)
			bsearch(0, seq) should equal(-1)
			bsearch(1, seq) should equal(0)
			bsearch(2, seq) should equal(-1)
			bsearch(Int.MaxValue, seq) should equal(-1)
		}
		given(Seq(1, 2)) { seq =>
			bsearch(Int.MinValue, seq) should equal(-1)
			bsearch(0, seq) should equal(-1)
			bsearch(1, seq) should equal(0)
			bsearch(2, seq) should equal(1)
			bsearch(3, seq) should equal(-1)
			bsearch(Int.MaxValue, seq) should equal(-1)
		}
		given(Seq(1, 2, 3)) { seq =>
			bsearch(Int.MinValue, seq) should equal(-1)
			bsearch(0, seq) should equal(-1)
			bsearch(1, seq) should equal(0)
			bsearch(2, seq) should equal(1)
			bsearch(3, seq) should equal(2)
			bsearch(4, seq) should equal(-1)
			bsearch(Int.MaxValue, seq) should equal(-1)
		}
		given(Seq(1, 2, 3, 4)) { seq =>
			bsearch(Int.MinValue, seq) should equal(-1)
			bsearch(0, seq) should equal(-1)
			bsearch(1, seq) should equal(0)
			bsearch(2, seq) should equal(1)
			bsearch(3, seq) should equal(2)
			bsearch(4, seq) should equal(3)
			bsearch(5, seq) should equal(-1)
			bsearch(Int.MaxValue, seq) should equal(-1)
		}
	}

	private def bsearch[T](value: T, seq: Seq[T], shift: Int = 0)(implicit orderer: T => Ordered[T]): Int = {
		if (seq.isEmpty) return -1

		val midPos: Int = seq.size / 2
		if (value < seq(midPos)) {
			bsearch(value, seq.view(0, midPos), shift)
		} else if (value > seq(midPos)) {
			bsearch(value, seq.view(midPos + 1, seq.size), shift + midPos + 1)
		} else {
			midPos + shift
		}
	}

	private def given[T](seq: Seq[T])(f: (Seq[T] => Any)) {
		f(seq)
	}
}