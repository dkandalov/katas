package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 23/07/2012
 */

class EightQueen7 extends ShouldMatchers {
	@Test def shouldSolveForBoardOfSize_4() {
		solveForBoard(4).size should equal(5)
	}

	def solveForBoard(boardSize: Int): Seq[Seq[Int]] = {
		Seq()
	}

	@Test def shouldConvertSolutionToAPrintableBoard() {
		val solution = Seq()
		asBoard(4, solution) should equal("")
	}

	def asBoard(boardSize: Int, solution: Seq[(Int, Int)]): String = {
		val board = for (row <- 0 to boardSize; col <- 0 to boardSize) yield {
			if (col == boardSize) "X\n" else "X"
		}
		board.mkString(",")
	}
}