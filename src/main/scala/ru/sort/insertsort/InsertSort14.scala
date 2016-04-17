package ru.sort.insertsort

import org.scalatest.Matchers
import ru.sort.SeqSortTest

import scala.reflect.ClassTag


class InsertSort14 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def insert(sorted: Seq[T], element: T): Seq[T] = {
			if (sorted.isEmpty) return Seq(element)
			if (sorted.head >= element) element +: sorted
			else sorted.head +: insert(sorted.tail, element)
		}
		def insertSort(sorted: Seq[T], seq: Seq[T]): Seq[T] = {
			if (seq.isEmpty) sorted
			else insertSort(insert(sorted, seq.head), seq.tail)
		}
		insertSort(Seq(), seq)
	}
}