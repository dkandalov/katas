package katas.scala.sort.quicksort

import org.scalatest.Matchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class QuickSort6 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		if (seq.isEmpty) return seq
		val pivot = seq(seq.size / 2)

		sort(seq.filter(_ < pivot)) ++
			seq.filter(_ == pivot) ++
			sort(seq.filter(_ > pivot))
	}
}