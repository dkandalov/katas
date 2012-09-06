package ru.connectivity

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 06/09/2012
 */

class WUnion extends ShouldMatchers {
	@Test def determineIfPointAreConnected_InSimplestCases() {
		val board = new Board(2)

		board.areConnected(0, 0) should equal(true)
		board.connect(0, 0) should equal(true)
		board.areConnected(0, 0) should equal(true)
	}

	class Board(size: Int) {
		def connect(p1: Int, p2: Int) {

		}

		def areConnected(p1: Int, p2: Int) = {
			false
		}
	}
}