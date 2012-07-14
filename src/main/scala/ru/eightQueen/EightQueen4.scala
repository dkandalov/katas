package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 14/07/2012
 */

class EightQueen4 extends ShouldMatchers {

	@Test def shouldFindSolutionForBoardOfSize_4() {
		val solution = solveForBoardWithSize(4)
		println(solution)
	}

	def solveForBoardWithSize(size: Int) {

	}
}