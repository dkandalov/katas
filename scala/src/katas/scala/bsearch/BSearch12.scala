package katas.scala.bsearch

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 06/06/2012
 */

class BSearch12 extends Matchers {
	import BinarySearch._

	@Test def shouldProgressFromOneStateOfBinarySearchToAnother() {
		State(0, Seq(), 0, None).next should equal(State(0, Seq(), 0, Some(None)))
		State(1, Seq(), 0, None).next should equal(State(1, Seq(), 0, Some(None)))

		State(0, Seq(1), 0, None).next should equal(State(0, Seq(), 0, None))
		State(1, Seq(1), 0, None).next should equal(State(1, Seq(1), 0, Some(Some(0))))
		State(2, Seq(1), 0, None).next should equal(State(2, Seq(), 1, None))

		State(0, Seq(1, 2), 0, None).next should equal(State(0, Seq(1), 0, None))
		State(1, Seq(1, 2), 0, None).next should equal(State(1, Seq(1), 0, None))
		State(2, Seq(1, 2), 0, None).next should equal(State(2, Seq(1, 2), 0, Some(Some(1))))
		State(3, Seq(1, 2), 0, None).next should equal(State(3, Seq(), 2, None))

		State(0, Seq(1, 2, 3), 0, None).next should equal(State(0, Seq(1), 0, None))
		State(1, Seq(1, 2, 3), 0, None).next should equal(State(1, Seq(1), 0, None))
		State(2, Seq(1, 2, 3), 0, None).next should equal(State(2, Seq(1, 2, 3), 0, Some(Some(1))))
		State(3, Seq(1, 2, 3), 0, None).next should equal(State(3, Seq(3), 2, None))

		State(3, Seq(3), 2, None).next should equal(State(3, Seq(3), 2, Some(Some(2))))
	}

	@Test def shouldFindIndexOfAnElementInASequence() {
		binarySearch(0, Seq()) should equal(None)

		binarySearch(0, Seq(1)) should equal(None)
		binarySearch(1, Seq(1)) should equal(Some(0))
		binarySearch(2, Seq(1)) should equal(None)

		binarySearch(0, Seq(1, 2)) should equal(None)
		binarySearch(1, Seq(1, 2)) should equal(Some(0))
		binarySearch(2, Seq(1, 2)) should equal(Some(1))
		binarySearch(3, Seq(1, 2)) should equal(None)

		binarySearch(0, Seq(1, 2, 3)) should equal(None)
		binarySearch(1, Seq(1, 2, 3)) should equal(Some(0))
		binarySearch(2, Seq(1, 2, 3)) should equal(Some(1))
		binarySearch(3, Seq(1, 2, 3)) should equal(Some(2))
		binarySearch(4, Seq(1, 2, 3)) should equal(None)
	}

	object BinarySearch {
		def binarySearch[T](value: T, seq: Seq[T])(implicit orderer: T => Ordered[T]): Option[Int] = {
			doBinarySearch(State(value, seq, 0, None))
		}

		private def doBinarySearch[T](state: State[T])(implicit orderer: T => Ordered[T]): Option[Int] = {
			if (state.result.isDefined) state.result.get
			else doBinarySearch(state.next())
		}

		case class State[T](value: T, seq: Seq[T], shift: Int, result: Option[Option[Int]])(implicit orderer: T => Ordered[T]) {
			def next(): State[T] = {
				if (seq.isEmpty) return this.withSomeResult(None)

				val midPos = seq.size / 2
				val midValue = seq(midPos)

				if (value == midValue) this.withSomeResult(Some(shift + midPos))
				else if (value < midValue) this.withSeq(seq.slice(0, midPos))
				else this.withSeq(seq.slice(midPos + 1, seq.size)).withShift(midPos + 1)
			}

			private def withSomeResult(result: Option[Int]) = State(value, seq, shift, Some(result))
			private def withSeq(seq: Seq[T]) = State(value, seq, shift, result)
			private def withShift(shift: Int) = State(value, seq, shift, result)
		}
	}
}