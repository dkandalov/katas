package ru.sort.insertsort

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * User: DKandalov
 */

class InsertSort extends AssertionsForJUnit {
	@Test
	def shouldSortLists() {
		assert(sort(List()) === List())
		assert(sort(List(1)) === List(1))
		assert(sort(List(2, 1)) === List(1, 2))
		assert(sort(List(2, 3, 1)) === List(1, 2, 3))
	}

	def sort(list: List[Int]): List[Int] = {
		def ssort(sorted: List[Int], inputList: List[Int]): List[Int] = {
			inputList match {
				case List() => sorted // returned inputList instead of sorted :(
				case List(x, _*) => ssort(insert(sorted, inputList.head), inputList.tail)
			}
		}

		ssort(List(), list)
	}

	def insert(sorted: List[Int], value: Int): List[Int] = {
		var i = 0
		while (i < sorted.length && sorted(i) < value) i = i + 1 // forgot i < sorted.length
		val split = sorted.splitAt(i)
		split._1 ::: List(value) ::: split._2
	}

}