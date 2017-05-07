package katas.scala.bsearch

import org.junit.Test
import org.scalatest.Matchers

import scala.annotation.tailrec


class BinarySearch19 extends Matchers {
	@Test def findIndexOfElementInASequence() {
		search(1, Seq()) should equal(notFound)

		Seq(0, 1, 2).map{ search(_, Seq(1)) } should equal(Seq(notFound, 0, notFound))
		Seq(0, 1, 2, 3).map{ search(_, Seq(1, 2)) } should equal(Seq(notFound, 0, 1, notFound))
		Seq(0, 1, 2, 3, 4).map{ search(_, Seq(1, 2, 3)) } should equal(Seq(notFound, 0, 1, 2, notFound))
		Seq(0, 1, 2, 3, 4, 5).map{ search(_, Seq(1, 2, 3, 4)) } should equal(Seq(notFound, 0, 1, 2, 3, notFound))

		Seq('a', 'b', 'd', 'e').map{ search(_, Seq('b', 'd')) } should equal(Seq(notFound, 0, 1, notFound))
	}

	val notFound = -1

	@tailrec private def search[T](value: T, sequence: Seq[T], shift: Int = 0)(implicit order: T => Ordered[T]): Int = {
		if (sequence.isEmpty) return notFound

		val midIndex = sequence.size / 2
		val midValue = sequence(midIndex) // was "midValue"

		if (value == midValue) midIndex + shift
		else if (value < midValue) search(value, sequence.take(midIndex), shift)
		else search(value, sequence.drop(midIndex + 1), midIndex + shift + 1)
	}
}