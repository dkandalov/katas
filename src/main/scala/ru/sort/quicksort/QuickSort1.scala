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
			val pivotIndex = (from + to) / 2
			val pivot = array(pivotIndex)

			var fromIndex = from
			var toIndex = to
			while (fromIndex < toIndex) {
				val i1 = Range(from, pivotIndex).find(array(_) >= pivot).getOrElse(-1)
				val i2 = Range(pivotIndex + 1, to).find(array(_) <= pivot).getOrElse(-1)
				if (i1 == -1 || i2 == -1) fromIndex = toIndex
				else {
					swap(array, i1, i2)
					fromIndex = i1
					toIndex = i2
				}
			}
			swap(array, from, pivotIndex)

			quickSort(array, from, pivotIndex)
			quickSort(array, pivotIndex, array.length)
			array
		}

		if (seq.size <= 1) return seq
		quickSort(seq.toArray[T], 0, seq.size).toSeq
	}
}