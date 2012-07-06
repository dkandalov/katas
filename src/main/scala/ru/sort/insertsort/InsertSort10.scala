package ru.sort.insertsort

import org.scalatest.matchers.ShouldMatchers
import ru.sort.SeqSortTest

/**
 * User: dima
 * Date: 06/07/2012
 */

class InsertSort10 extends SeqSortTest with ShouldMatchers {
	def sort[T](seq: Seq[T])(implicit ordered: T => Ordered[T]): Seq[T] = {
		if (seq.isEmpty) seq
		else insert(seq.head, sort(seq.tail))
	}
	
	private def insert[T](value: T, seq: Seq[T])(implicit ordered: T => Ordered[T]): Seq[T] = {
		if (seq.isEmpty) Seq(value)
		else if (value <= seq.head) value +: seq
		else if (value > seq.head) seq.head +: insert(value, seq.tail)
		else throw new IllegalStateException()
	}
}