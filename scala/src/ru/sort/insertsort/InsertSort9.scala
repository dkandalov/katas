package ru.sort.insertsort

import org.scalatest.Matchers
import ru.sort.SeqSortTest

import scala.reflect.ClassTag

/**
 * User: dima
 * Date: 27/06/2012
 */


class InsertSort9 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {

		def insert(v: T, sorted: Seq[T]): Seq[T] = {
			if (sorted.isEmpty) Seq(v)
			else if (v <= sorted.head) v +: sorted
			else sorted.head +: insert(v, sorted.tail)
		}
		
		if (seq.isEmpty) seq
		else insert(seq.head, sort(seq.tail))
	}
}


class InsertSort9_ extends SeqSortTest with Matchers {
	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T]), tag: ClassTag[T]): Seq[T] = {
		def swap(s: Seq[T], i1: Int, i2: Int) = s.updated(i1, s(i2)).updated(i2, s(i1))

		var result = seq
		1.until(result.size).foreach{ i =>
			0.until(i).reverse.foreach{ j =>
				if (result(j) > result(j + 1)) {
					result = swap(result, j, j + 1)
				}
			}
		}
		result
	}
}