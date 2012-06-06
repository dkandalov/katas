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
	}

	def find(value: Int, seq: Seq[Int], shift: Int): Option[Int] = {
		None
	}

	def next(state: State): Option[State] = {
		if (state.seq.isEmpty) return Some(state)
		None
	}
}