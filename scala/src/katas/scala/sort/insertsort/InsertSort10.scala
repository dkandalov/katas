package katas.scala.sort.insertsort

import org.scalatest.Matchers
import katas.scala.sort.SeqSortTest
import collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * User: dima
 * Date: 06/07/2012
 */

class InsertSort10 extends SeqSortTest with Matchers {

	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		val array = ArrayBuffer(seq: _*)
		1.until(array.size).foreach{ i =>
			1.to(i).reverse.foreach{ j =>
				if (array(j - 1) > array(j)) {
					swap(array, j - 1, j)
				}
			}
		}
		array
	}

	private def swap[T](buffer: ArrayBuffer[T], i1: Int, i2: Int) {
		val tmp = buffer(i1)
		buffer(i1) = buffer(i2)
		buffer(i2) = tmp
	}

	def sort_[T](seq: Seq[T])(implicit ordered: T => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		if (seq.isEmpty) seq
		else insert(seq.head, sort(seq.tail))
	}
	
	private def insert[T](value: T, seq: Seq[T])(implicit ordered: T => Ordered[T]): Seq[T] = {
		if (seq.isEmpty) Seq(value)
		else if (value <= seq.head) value +: seq
		else if (value > seq.head) seq.head +: insert(value, seq.tail)
		else throw new IllegalStateException()
	}
}