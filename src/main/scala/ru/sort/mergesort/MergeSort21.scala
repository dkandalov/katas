package ru.sort.mergesort

import org.scalatest.Matchers
import ru.sort.SeqSortTest


class MergeSort21 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassManifest[T]): Seq[T] = {
		def merge(s1: Seq[T], s2: Seq[T]): Seq[T] = {
			if (s1.isEmpty) s2
			else if (s2.isEmpty) s1
			else if (s1.head < s2.head) s1.head +: merge(s1.tail, s2)
			else s2.head +: merge(s1, s2.tail)
		}

		if (seq.size <= 1) return seq
		val mid = seq.size / 2
		val (part1, part2) = seq.splitAt(mid)
		merge(sort(part1), sort(part2))
	}
}