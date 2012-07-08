package ru.sort.mergesort

import ru.sort.SeqSortTest
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 08/07/2012
 */

class MergeSort11 extends SeqSortTest with ShouldMatchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T]): Seq[T] = {
		def split(seq: Seq[T]): (Seq[T], Seq[T]) = {
			(Seq(), Seq())
		}

		def merge(seq1: Seq[T], seq2: Seq[T]): Seq[T] = {
			Seq()
		}

		val s = split(seq)
		merge(sort(s._1), sort(s._2))
	}
}