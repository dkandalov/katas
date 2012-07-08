package ru.sort.mergesort

import ru.sort.SeqSortTest
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 08/07/2012
 */

class MergeSort11 extends SeqSortTest with ShouldMatchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T]) = {
		Seq()
	}
}