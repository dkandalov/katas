package katas.scala.sort.quicksort

import org.scalatest.Matchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class QuickSort5 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		if (seq.size <= 1) return seq
		val pivot = seq.head
		(sort(seq.tail.filter(_ <= pivot)) :+ pivot) ++ sort(seq.tail.filter(_ > pivot))
	}
}