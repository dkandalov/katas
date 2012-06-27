package ru.sort.insertsort

import ru.util.Arcade
import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import ru.sort.SortTest

/**
 * User: dima
 * Date: 26/06/2012
 */

@Arcade
class InsertSort8 extends ShouldMatchers with SortTest {

	@Test def test() {
		shouldSortIntegerSequencesOfDifferentSize(this)
	}

	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T])): Seq[T] = {
		def insert(seq: Seq[T], n: T): Seq[T] = {
			if (seq.isEmpty) Seq(n)
			else if (seq.head >= n) n +: seq
			else seq.head +: insert(seq.tail, n)
		}

		if (seq.isEmpty) seq
		else insert(sort(seq.tail), seq.head)
	}
}