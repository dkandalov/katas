package katas.scala.bsearch

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.annotation.tailrec


class BinarySearch18 extends ShouldMatchers {

	@Test def findIndexOfElementInAList() {
		binarySearch(List(), 1) should equalTo(notFound)

		Range(0, 2).toList.map{ binarySearch(Seq(1), _)} should equalTo(Seq(notFound, 0, notFound))
		Range(0, 3).toList.map{ binarySearch(Seq(1, 2), _)} should equalTo(Seq(notFound, 0, 1, notFound))
		Range(0, 4).toList.map{ binarySearch(Seq(1, 2, 3), _)} should equalTo(Seq(notFound, 0, 1, 2, notFound))
		Range(0, 5).toList.map{ binarySearch(Seq(1, 2, 3, 4), _)} should equalTo(Seq(notFound, 0, 1, 2, 3, notFound))
	}

	private val notFound = -1

	@tailrec private def binarySearch(seq: Seq[Int], value: Int, shift: Int = 0): Int = {
		if (seq.isEmpty) return notFound

		val midIndex = seq.size / 2
		val midValue = seq(midIndex)
		if (value == midValue) midIndex + shift
		else if (value < midValue) binarySearch(seq.take(midIndex), value, shift)
		else binarySearch(seq.drop(midIndex + 1), value, shift + midIndex + 1)
	}
}