package katas.scala.sort.mergesort

import katas.scala.sort.SeqSortTest
import org.scalatest.Matchers

import scala.reflect.ClassTag

/**
 * User: dima
 * Date: 08/07/2012
 */

class MergeSort11 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def split(seq: Seq[T]): (Seq[T], Seq[T]) = {
			seq.splitAt(seq.size / 2)
		}

		def merge(seq1: Seq[T], seq2: Seq[T]): Seq[T] = {
			if (seq1.isEmpty) return seq2
			if (seq2.isEmpty) return seq1

			if (seq1.head <= seq2.head)
				seq1.head +: merge(seq1.tail, seq2)
			else
				seq2.head +: merge(seq1, seq2.tail)
		}

		if (seq.size < 2) return seq

		val s = split(seq)
		merge(sort(s._1), sort(s._2))
	}
}