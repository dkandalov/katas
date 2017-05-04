package katas.scala.bsearch

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 05/08/2012
 */

class BSearch14 extends ShouldMatchers {
//	@Test def () {
//	}

	@Test def shouldFindIndexOfAnElementInASequence() {
		search(0, Seq()) should equalTo(None)

		search(0, Seq(1)) should equalTo(None)
		search(1, Seq(1)) should equalTo(Some(0))
		search(2, Seq(1)) should equalTo(None)

		search(0, Seq(1, 2)) should equalTo(None)
		search(1, Seq(1, 2)) should equalTo(Some(0))
		search(2, Seq(1, 2)) should equalTo(Some(1))
		search(3, Seq(1, 2)) should equalTo(None)

		search(0, Seq(1, 2, 3)) should equalTo(None)
		search(1, Seq(1, 2, 3)) should equalTo(Some(0))
		search(2, Seq(1, 2, 3)) should equalTo(Some(1))
		search(3, Seq(1, 2, 3)) should equalTo(Some(2))
		search(4, Seq(1, 2, 3)) should equalTo(None)

		search("a", Seq("a", "b", "c")) should equalTo(Some(0))
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