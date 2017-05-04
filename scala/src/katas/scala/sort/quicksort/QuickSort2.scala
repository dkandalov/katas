package katas.scala.sort.quicksort

import org.specs2.matcher.ShouldMatchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class QuickSort2 extends SeqSortTest with ShouldMatchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		if (seq.size <= 1) return seq
		val pivot = seq(seq.size / 2)
		sort(seq.filter(_ < pivot)) ++ seq.filter(_ == pivot) ++ sort(seq.filter(_ > pivot))
	}
}