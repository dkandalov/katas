package katas.scala.bsearch

import org.junit.Test
import org.scalatest.Matchers

import scala.annotation.tailrec

/**
 * User: dima
 * Date: 18/06/2012
 */

class BSearch13 extends Matchers {
	@Test def should_find_element_index_in_a_sequence() {
		bsearch(1, Seq()) should equal(None)

		bsearch(0, Seq(1)) should equal(None)
		bsearch(1, Seq(1)) should equal(Some(0))
		bsearch(2, Seq(1)) should equal(None)

		bsearch(0, Seq(1, 2)) should equal(None)
		bsearch(1, Seq(1, 2)) should equal(Some(0))
		bsearch(2, Seq(1, 2)) should equal(Some(1))
		bsearch(3, Seq(1, 2)) should equal(None)

		bsearch(0, Seq(1, 2, 3)) should equal(None)
		bsearch(1, Seq(1, 2, 3)) should equal(Some(0))
		bsearch(2, Seq(1, 2, 3)) should equal(Some(1))
		bsearch(3, Seq(1, 2, 3)) should equal(Some(2))
		bsearch(4, Seq(1, 2, 3)) should equal(None)
	}

	@Test def should_go_from_one_state_of_binary_search_to_another() {
		State(1, Seq(), 0).next() should equal(State(1, Seq(), 0, Some(None)))

		State(0, Seq(1), 0).next() should equal(State(0, Seq(), 0, None))
		State(1, Seq(1), 0).next() should equal(State(1, Seq(1), 0, Some(Some(0))))
		State(2, Seq(1), 0).next() should equal(State(2, Seq(), 1, None))

		State(0, Seq(1, 2), 0).next() should equal(State(0, Seq(1), 0, None))
		State(1, Seq(1, 2), 0).next() should equal(State(1, Seq(1), 0, None))
		State(2, Seq(1, 2), 0).next() should equal(State(2, Seq(1, 2), 0, Some(Some(1))))
		State(3, Seq(1, 2), 0).next() should equal(State(3, Seq(), 2, None))

		State(0, Seq(1, 2, 3), 0).next() should equal(State(0, Seq(1), 0, None))
		State(1, Seq(1, 2, 3), 0).next() should equal(State(1, Seq(1), 0, None))
		State(2, Seq(1, 2, 3), 0).next() should equal(State(2, Seq(1, 2, 3), 0, Some(Some(1))))
		State(3, Seq(1, 2, 3), 0).next() should equal(State(3, Seq(3), 2, None))
		State(4, Seq(1, 2, 3), 0).next() should equal(State(4, Seq(3), 2, None))

		State(1, Seq(1), 3).next() should equal(State(1, Seq(1), 3, Some(Some(3))))
	}

	case class State(n: Int, values: Seq[Int], shift: Int, result: Option[Option[Int]] = None) {
		def next(): State = {
			if (values.isEmpty) return this.withResult(Some(None))

			val midPos = values.size / 2
			val midValue = values(midPos)
			
			if (n == midValue) this.withResult(Some(Some(shift + midPos)))
			else if (n < midValue) this.withValues(values.slice(0, midPos))
			else this.withValues(values.slice(midPos + 1, values.size)).withShift(shift + midPos + 1)
		}
		
		private def withResult(newResult: Option[Option[Int]]) = State(n, values, shift, newResult)
		private def withValues(newValues: Seq[Int]) = State(n, newValues, shift, result)
		private def withShift(newShift: Int) = State(n, values, newShift, result)
	}

	def bsearch(n: Int, values: Seq[Int]): Option[Int] = {
		doSearch(State(n, values, 0, None))
	}

	@tailrec private def doSearch(state: State): Option[Int] = {
		if (state.result.isEmpty) doSearch(state.next())
		else state.result.get
	}
}