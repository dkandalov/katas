package ru.sort

import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 27/06/2012
 */

trait SortTest extends ShouldMatchers {

	type SequenceSort = {def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T])): Seq[T]}

	def shouldSortIntegerSequencesOfDifferentSize(sequenceSort: SequenceSort) {
		sequenceSort.sort(Seq()) should equal(Seq())
		sequenceSort.sort(Seq(1)) should equal(Seq(1))
		sequenceSort.sort(Seq(1, 2)) should equal(Seq(1, 2))
		sequenceSort.sort(Seq(2, 1)) should equal(Seq(1, 2))
		sequenceSort.sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
		sequenceSort.sort(Seq(2, 1, 3)) should equal(Seq(1, 2, 3))
		sequenceSort.sort(Seq(3, 1, 2)) should equal(Seq(1, 2, 3))

		1.to(8).map(Range(1, _).toSeq).foreach {
			seq =>
				seq.permutations.foreach {
					perm =>
						sequenceSort.sort(perm) should equal(seq)
				}
		}
	}

}