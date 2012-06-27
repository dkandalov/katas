package ru.sort.insertsort

import ru.sort.SortTest
import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import ru.util.Arcade

/**
 * User: dima
 * Date: 27/06/2012
 */

@Arcade
class InsertSort9 extends SortTest with ShouldMatchers {
	@Test def sort() {
		shouldSortIntegerSequencesOfDifferentSize(this)
	}

	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T])): Seq[T] = {
		Seq()
	}
}