package ru.sort

import org.scalatest.Matchers
import org.junit.Test

trait SeqSortTest extends Matchers {

	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T])): Seq[T]

	@Test def shouldSortIntegerSequencesOfDifferentSize() {
		sort(Seq()) should equal(Seq())
		sort(Seq(1)) should equal(Seq(1))
		sort(Seq(1, 2)) should equal(Seq(1, 2))
		sort(Seq(2, 1)) should equal(Seq(1, 2))
		sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(1, 3, 2)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 1, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 3, 1)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 1, 2)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 2, 1)) should equal(Seq(1, 2, 3))

		1.to(8).map(Range(1, _).toSeq).foreach { seq =>
				seq.permutations.foreach { perm =>
						sort(perm) should equal(seq)
				}
		}
	}

}