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
		
		def insert(v: T, sorted: Seq[T]): Seq[T] = {
			if (sorted.isEmpty) Seq(v)
			else if (v <= sorted.head) v +: sorted
			else sorted.head +: insert(v, sorted.tail)
		}
		
		if (seq.isEmpty) seq
		else insert(seq.head, sort(seq.tail))
	}
}

@Arcade
class InsertSort9_ extends SortTest with ShouldMatchers {
	@Test def sort() {
		shouldSortIntegerSequencesOfDifferentSize(this)
	}

	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T])): Seq[T] = {
		Seq()
	}
}