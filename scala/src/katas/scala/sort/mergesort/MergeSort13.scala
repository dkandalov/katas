package katas.scala.sort.mergesort

import org.scalatest.Matchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class MergeSort13 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		if (seq.size <= 1) seq
		else {
			val (part1, part2) = seq.splitAt(middleOf(seq))
			merge(sort(part1), sort(part2))
		}
	}

	private def merge[T](seq1: Seq[T], seq2: Seq[T])(implicit ordering: (T) => Ordered[T]): Seq[T] = {
		if (seq1.isEmpty) seq2
		else if (seq2.isEmpty) seq1
		else if (seq1.head <= seq2.head) seq1.head +: merge(seq1.tail, seq2)
		else seq2.head +: merge(seq1, seq2.tail)
	}

	private def middleOf[T](seq: Seq[T]): Int = {
		seq.size / 2
	}
}