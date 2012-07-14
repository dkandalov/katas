package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 14/07/2012
 */

class EightQueen4 extends ShouldMatchers {
	type Position = (Int, Int)
	type Solution = Seq[Position]

	@Test def shouldFindSolutionForBoardOfSize_4() {
		val solution = solveForBoardOfSize(4)
		println(asPrintableSolution(solution, 4))
	}

	def solveForBoardOfSize(size: Int): Seq[Solution] = {
		Seq()
	}

	def asPrintableSolution(solutions: Seq[Solution], boardSize: Int): String = {
		""
	}

	def asPrintableBoard(seq: Seq[Position], boardSize: Int): String = {
		Range(0, boardSize).map { row =>
			Range(0, boardSize).map { col =>
				if (seq.contains((row, col))) "0" else "X"
			}
		}.mkString("\n")
	}
}