package ru.bsearch

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 05/08/2012
 */

class BSearch14 extends Matchers {
//	@Test def () {
//	}

	@Test def shouldFindIndexOfAnElementInASequence() {
		search(0, Seq()) should equal(None)

		search(0, Seq(1)) should equal(None)
		search(1, Seq(1)) should equal(Some(0))
		search(2, Seq(1)) should equal(None)

		search(0, Seq(1, 2)) should equal(None)
		search(1, Seq(1, 2)) should equal(Some(0))
		search(2, Seq(1, 2)) should equal(Some(1))
		search(3, Seq(1, 2)) should equal(None)

		search(0, Seq(1, 2, 3)) should equal(None)
		search(1, Seq(1, 2, 3)) should equal(Some(0))
		search(2, Seq(1, 2, 3)) should equal(Some(1))
		search(3, Seq(1, 2, 3)) should equal(Some(2))
		search(4, Seq(1, 2, 3)) should equal(None)

		search("a", Seq("a", "b", "c")) should equal(Some(0))
	}

	def search[T](value: T/*Int*/, seq: Seq[T/*Int*/])(implicit orderer: T => Ordered/*ing*/[T]): Option[Int] = {
		var from = 0
		var to = seq.size
		var midPos = (from + to) / 2

		while (from < to) {
			if (value == seq(midPos)) return Some(midPos)
			else if (value < seq(midPos)) /*from*/ to = midPos
			else from = midPos/*t*/ + 1
			midPos = (from + to) / 2
		}
		None
	}

	def search_(value: Int, seq: Seq[Int]): Option[Int] = {
		var from = 0
		var to = seq.size
		var midPos = (from + to) / 2

		while (from < to) {
			if (seq(midPos) == value) return Some(midPos)
			else if (seq(midPos) > value) {
				to = midPos// - 1
			} else if (/*seqmi*//*seqMid*/seq(midPos/*t*/) < /*>*/ value) {
				from = midPos + 1// + 1
			}
			midPos = (from + to) / 2
			//println(midPos)
		}
		//-1
		None/*()*/
	}
}