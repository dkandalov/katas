package katas.scala.sort.quicksort

import org.specs2.matcher.ShouldMatchers
import katas.scala.sort.SeqSortTest

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class QuickSort1 extends SeqSortTest with ShouldMatchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def swap(array: Array[T], i1: Int, i2: Int): Array[T] = {
			val temp = array(i1)
			array(i1) = array(i2)
			array(i2) = temp
			array
		}
		def quickSort(array: Array[T], from: Int, to: Int): Array[T] = {
			if (to - from <= 1) return array
			val pivotIndex = to - 1
			val pivot = array(pivotIndex)

			var i1 = from
			var i2 = pivotIndex
			while (i1 < i2) {
				i1 = findIndex(array, i1, pivotIndex){ _ >= pivot }
				i2 = findIndex(array, i2 - 1, i1){ _ <= pivot }
				if (i1 < i2) {
					swap(array, i1, i2)
				}
			}
			swap(array, i1, pivotIndex)

			quickSort(array, from, i1)
			quickSort(array, i1 + 1, to)
			array
		}

		if (seq.size <= 1) return seq
		quickSort(seq.toArray[T], 0, seq.size).toSeq
	}

	def findIndex[T](array: Array[T], from: Int, to: Int)(f: T => Boolean): Int = {
		var i = from
		if (from < to) {
			while (i <= to && !f(array(i))) i = i + 1
		} else {
			while (i >= to && !f(array(i))) i = i - 1
		}
		i
	}
}