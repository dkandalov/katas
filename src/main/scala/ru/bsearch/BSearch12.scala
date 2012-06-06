package ru.bsearch

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 06/06/2012
 */

class BSearch12 extends ShouldMatchers {
	case class State(value: Int, seq: Seq[Int], shift: Int, pos: Option[Option[Int]]) {
		def withPos(pos: Option[Option[Int]]) = State(value, seq, shift, pos)
		def withSeq(seq: Seq[Int]) = State(value, seq, shift, pos)
		def withShift(shift: Int) = State(value, seq, shift, pos)
	}
	
	@Test def aaa() {
		next(State(1, Seq(), 0, None)) should equal(State(1, Seq(), 0, Some(None)))

		next(State(1, Seq(1), 0, None)) should equal(State(1, Seq(1), 0, Some(Some(0))))
		next(State(0, Seq(1), 0, None)) should equal(State(1, Seq(), 0, Some(None)))
		next(State(2, Seq(1), 0, None)) should equal(State(1, Seq(), 1, Some(None)))
	}

	def find(value: Int, seq: Seq[Int], shift: Int): Option[Int] = {
		None
	}

	def next(state: State): State = {
		if (state.seq.isEmpty) return state.withPos(Some(None))

		val midPos = state.seq.size / 2
		val midValue = state.seq(midPos)

		if (state.value == midValue) return state.withPos(Some(Some(midPos)))

		state
	}
}