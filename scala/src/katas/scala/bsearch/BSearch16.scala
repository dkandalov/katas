package katas.scala.bsearch

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 16/09/2012
 */

class BSearch16 extends Matchers {
	@Test def findIndexOfElementInASequence() {
		given(Seq[Int]()) { seq =>
				bsearch(1, seq) should equal((Seq(), -1))
		}
		given(Seq(1)) { seq =>
				bsearch(Int.MinValue, seq) should equal((Seq(0), -1))
				bsearch(0, seq) should equal((Seq(0), -1))
				bsearch(1, seq) should equal((Seq(0), 0))
				bsearch(2, seq) should equal((Seq(0), -1))
				bsearch(Int.MaxValue, seq) should equal((Seq(0), -1))
		}
		given(Seq(1, 2)) { seq =>
				bsearch(Int.MinValue, seq) should equal((Seq(1, 0), -1))
				bsearch(0, seq) should equal((Seq(1, 0), -1))
				bsearch(1, seq) should equal((Seq(1, 0), 0))
				bsearch(2, seq) should equal((Seq(1), 1))
				bsearch(3, seq) should equal((Seq(1), -1))
				bsearch(Int.MaxValue, seq) should equal((Seq(1), -1))
		}
		given(Seq(1, 2, 3)) { seq =>
				bsearch(Int.MinValue, seq) should equal((Seq(1, 0), -1))
				bsearch(0, seq) should equal((Seq(1, 0), -1))
				bsearch(1, seq) should equal((Seq(1, 0), 0))
				bsearch(2, seq) should equal((Seq(1), 1))
				bsearch(3, seq) should equal((Seq(1, 2), 2))
				bsearch(4, seq) should equal((Seq(1, 2), -1))
				bsearch(Int.MaxValue, seq) should equal((Seq(1, 2), -1))
		}
		given(Seq(1, 2, 3, 4)) { seq =>
				bsearch(Int.MinValue, seq) should equal((Seq(2, 1, 0), -1))
				bsearch(0, seq) should equal((Seq(2, 1, 0), -1))
				bsearch(1, seq) should equal((Seq(2, 1, 0), 0))
				bsearch(2, seq) should equal((Seq(2, 1), 1))
				bsearch(3, seq) should equal((Seq(2), 2))
				bsearch(4, seq) should equal((Seq(2, 3), 3))
				bsearch(5, seq) should equal((Seq(2, 3), -1))
				bsearch(Int.MaxValue, seq) should equal((Seq(2, 3), -1))
		}
	}

	private def bsearch(value: Int, seq: Seq[Int], shift: Int = 0, steps: Seq[Int] = Seq()): (Seq[Int], Int) = {
		if (seq.isEmpty) return (steps, -1)

		val midPos = seq.size / 2
		if (value < seq(midPos)) {
			bsearch(value, seq.view(0, midPos), shift, steps :+ (shift + midPos))
		} else if (value > seq(midPos)) {
			bsearch(value, seq.view(midPos + 1, seq.size), shift + midPos + 1, steps :+ (shift + midPos))
		} else {
			(steps :+ (midPos + shift), midPos + shift)
		}
	}

	private def given[T](seq: Seq[T])(f: (Seq[T] => Any)) {
		f(seq)
	}
}