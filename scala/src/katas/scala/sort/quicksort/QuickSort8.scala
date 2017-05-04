package katas.scala.sort.quicksort

import org.specs2.matcher.ShouldMatchers
import katas.scala.sort.SeqSortTest

import scala.reflect.ClassTag


class QuickSort8 extends SeqSortTest with ShouldMatchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def sort(stream: Stream[T]): Stream[T] = {
			if (stream.isEmpty) return stream
			val pivot = stream.head

			sort(stream.filter(_ < pivot)) ++
				stream.filter(_ == pivot) ++
				sort(stream.filter(_ > pivot))
		}
		sort(seq.toStream)
	}
}