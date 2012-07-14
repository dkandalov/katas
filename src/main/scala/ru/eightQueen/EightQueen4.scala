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
		val solution = solveForBoardOfSize(4)
		println(asPrintableBoard(solution, 4))
	}

	def solveForBoardOfSize(size: Int): Seq[Position] = {
		Seq()
	}

	def asPrintableBoard(seq: Seq[Position], boardSize: Int): String = {
		Range(0, boardSize).map { row =>
			Range(0, boardSize).map { col =>
				if (seq.contains((row, col))) "0" else "X"
			}
		}.mkString("\n")
	}
}