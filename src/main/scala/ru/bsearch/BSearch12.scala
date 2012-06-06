package ru.bsearch

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 06/06/2012
 */

class BSearch12 extends ShouldMatchers {
	case class State(value: Int, seq: Seq[Int], shift: Int, pos: Option[Int])
	
	@Test def aaa() {
		next(State(1, Seq(), 0, None)) should equal(None)

		next(State(0, Seq(1), 0, None)) should equal(State(1, Seq(), 0, None))
		next(State(1, Seq(1), 0, None)) should equal(State(1, Seq(1), 0, Some(1)))
		next(State(2, Seq(1), 0, None)) should equal(State(1, Seq(), 1, None))
	}

	def find(value: Int, seq: Seq[Int], shift: Int): Option[Int] = {
		None
	}

	def next(state: State): Option[State] = {
		if (state.seq.isEmpty) return Some(state)
		None
	}
}