package ru.sort.quicksort

import org.scalatest.Matchers
import ru.sort.SeqSortTest

import scala.reflect.ClassTag


class QuickSort4 extends SeqSortTest with Matchers {
	override def sort[T](seq: Seq[T])(implicit ordered: (T) => Ordered[T], tag: ClassTag[T]): Seq[T] = {
		def swap(array: Array[T], i1: Int, i2: Int): Unit = {
			if (i1 == i2) return
			val tmp = array(i1)
			array(i1) = array(i2)
			array(i2) = tmp
		}
		def findIndex(array: Array[T], from: Int, to: Int)(f: T => Boolean): Int = {
			var i = from
			val step = if (from < to) 1 else -1
			do {
				i = i + step
			} while (i != to && !f(array(i)))
			i
		}
		def quickSort(array: Array[T], from: Int, to: Int): Array[T] = {
			if (to - from <= 1) return array
			if (to - from == 2) {
				if (array(from) > array(to - 1)) swap(array, from, to - 1)
				return array
			}
			val pivotIndex = to - 1
			val pivot = array(pivotIndex)

			var i1 = from - 1
			var i2 = pivotIndex
			while (i1 < i2) {
				i1 = findIndex(array, i1, pivotIndex){ _ >= pivot }
				i2 = findIndex(array, i2, from){ _ <= pivot }
				if (i1 < i2) {
					swap(array, i1, i2)
				}
			}
			swap(array, i1, pivotIndex)

			quickSort(array, from, i1)
			quickSort(array, i1 + 1, to)
			array
		}
		quickSort(seq.toArray[T], 0, seq.size).toSeq
	}

}