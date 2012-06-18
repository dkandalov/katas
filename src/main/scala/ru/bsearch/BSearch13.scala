package ru.bsearch

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 18/06/2012
 */

class BSearch13 extends ShouldMatchers {
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

	case class State(n: Int, values: Seq[Int], shift: Int, result: Option[Option[Int]])

	def bsearch(n: Int, values: Seq[Int]): Option[Int] = {
		var state = State(n, values, 0, None)
		while (state.result == None) {
			state = doBinarySearch(state.n, state.values, state.shift)
		}
		state.result.get
	}

	@Test def should_go_from_one_state_of_binary_search_to_another() {
		doBinarySearch(1, Seq(), 0) should equal(State(1, Seq(), 0, Some(None)))

		doBinarySearch(0, Seq(1), 0) should equal(State(0, Seq(), 0, None))
		doBinarySearch(1, Seq(1), 0) should equal(State(1, Seq(1), 0, Some(Some(0))))
		doBinarySearch(2, Seq(1), 0) should equal(State(2, Seq(), 1, None))

		doBinarySearch(0, Seq(1, 2), 0) should equal(State(0, Seq(1), 0, None))
		doBinarySearch(1, Seq(1, 2), 0) should equal(State(1, Seq(1), 0, None))
		doBinarySearch(2, Seq(1, 2), 0) should equal(State(2, Seq(1, 2), 0, Some(Some(1))))
		doBinarySearch(3, Seq(1, 2), 0) should equal(State(3, Seq(), 2, None))

		doBinarySearch(0, Seq(1, 2, 3), 0) should equal(State(0, Seq(1), 0, None))
		doBinarySearch(1, Seq(1, 2, 3), 0) should equal(State(1, Seq(1), 0, None))
		doBinarySearch(2, Seq(1, 2, 3), 0) should equal(State(2, Seq(1, 2, 3), 0, Some(Some(1))))

	}

	def doBinarySearch(n: Int, values: Seq[Int], shift: Int): State = {
		if (values.isEmpty) return State(n, values, shift, Some(None))

		val midPos = values.size / 2
		if (n == values(midPos)) State(n, values, shift, Some(Some(midPos)))
		else if (n < values(midPos)) State(n, values.slice(0, midPos), shift, None)
		else State(n, values.slice(midPos + 1, values.size), shift + midPos + 1, None)
	}
}