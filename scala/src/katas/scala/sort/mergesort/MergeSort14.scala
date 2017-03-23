package katas.scala.sort.mergesort

import org.scalatest.Matchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class MergeSort14 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def merge(seq1: Seq[T], seq2: Seq[T]): Seq[T] = {
			if (seq1.isEmpty) seq2
			else if (seq2.isEmpty) seq1
			else if (seq1.head <= seq2.head) seq1.head +: merge(seq1.tail, seq2)
			else seq2.head +: merge(seq1, seq2.tail)
		}

		if (seq.size <= 1) return seq
		val split = seq.splitAt(seq.size / 2)
		merge(sort(split._1), sort(split._2))
	}
}