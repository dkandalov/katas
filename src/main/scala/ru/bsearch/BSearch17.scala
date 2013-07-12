package ru.bsearch

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.annotation.tailrec


class BSearch17 extends ShouldMatchers {
	@Test def findIndexOfElementInASequence() {
		binarySearch(List(), 1) should equal(notFound)

		binarySearch(List(1), 0) should equal(notFound)
		binarySearch(List(1), 1) should equal(0)
		binarySearch(List(1), 2) should equal(notFound)

		binarySearch(List(1, 2), 0) should equal(notFound)
		binarySearch(List(1, 2), 1) should equal(0)
		binarySearch(List(1, 2), 2) should equal(1)
		binarySearch(List(1, 2), 3) should equal(notFound)

		binarySearch(List(1, 2, 3), 0) should equal(notFound)
		binarySearch(List(1, 2, 3), 1) should equal(0)
		binarySearch(List(1, 2, 3), 2) should equal(1)
		binarySearch(List(1, 2, 3), 3) should equal(2)
		binarySearch(List(1, 2, 3), 4) should equal(notFound)

		binarySearch(List(1, 2, 3, 4), 0) should equal(notFound)
		binarySearch(List(1, 2, 3, 4), 1) should equal(0)
		binarySearch(List(1, 2, 3, 4), 2) should equal(1)
		binarySearch(List(1, 2, 3, 4), 3) should equal(2)
		binarySearch(List(1, 2, 3, 4), 4) should equal(3)
		binarySearch(List(1, 2, 3, 4), 5) should equal(notFound)
	}

	val notFound = -1

	@tailrec private def binarySearch(values: Seq[Int], n: Int, shift: Int = 0): Int = {
		if (values.isEmpty) return -1

		val midIndex = values.size / 2
		val midValue = values(midIndex)
		if (n == midValue) midIndex + shift
		else if (n < midValue) binarySearch(values.take(midValue - 1), n, shift)
		else binarySearch(values.drop(midValue), n, shift + midValue)
	}

}