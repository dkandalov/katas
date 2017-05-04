package katas.scala.sort.mergesort

import org.specs2.matcher.ShouldMatchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class MergeSort15 extends SeqSortTest with ShouldMatchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def merge(seq1: Seq[T], seq2: Seq[T]): Seq[T] = {
			if (seq1.isEmpty) seq2
			else if (seq2.isEmpty) seq1
			else if (seq1.head <= seq2.head) seq1.head +: merge(seq1.tail, seq2)
			else seq2.head +: merge(seq1, seq2.tail)
		}
		if (seq.size <= 1) return seq
		val (part1, part2) = seq.splitAt(seq.size / 2)
		merge(sort(part1), sort(part2))
	}
}