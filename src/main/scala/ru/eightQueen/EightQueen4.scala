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
		println(asPrintableBoard(solution, 4))
	}

	def asPrintableBoard(seq: Seq[Position], boardSize: Int): String = {
		val board = Seq.fill(boardSize) { Seq.fill(boardSize) }
		""
	}

	def solveForBoardWithSize(size: Int): Seq[Position] = {
		Seq()
	}
}