package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 14/07/2012
 */

class EightQueen4 extends ShouldMatchers {
	type Position = (Int, Int)

	@Test def shouldFindSolutionForBoardOfSize_4() {
		val solution = solveForBoardWithSize(4)
		println(asPrintableBoard(solution))
	}

	def asPrintableBoard(seq: Seq[Position]): String = {
		""
	}

	def solveForBoardWithSize(size: Int): Seq[Position] = {
		Seq()
	}
}