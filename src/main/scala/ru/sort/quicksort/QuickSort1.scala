package ru.sort.quicksort

import org.scalatest.Matchers
import ru.sort.SeqSortTest

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class QuickSort1 extends SeqSortTest with Matchers {
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
			while (i1 < pivotIndex && i1 < i2) {
				var i = i1
				while (i <= pivotIndex && array(i) < pivot) { i = i + 1 }
				i1 = i
				// i1 = Range(i1, pivotIndex).find(array(_) >= pivot).getOrElse(pivotIndex)

				i = i2 - 1
				while (i >= i1 && array(i) > pivot) { i = i - 1 }
				i2 = i
				// i2 = Range(i1, i2).reverse.find(array(_) <= pivot).getOrElse(i1)

				if (i1 < pivotIndex && i1 < i2) {
					swap(array, i1, i2)
				}
			}
			swap(array, i1, pivotIndex)

			quickSort(array, from, pivotIndex)
			quickSort(array, pivotIndex, array.length)
			array
		}

		if (seq.size <= 1) return seq
		quickSort(seq.toArray[T], 0, seq.size).toSeq
	}
}