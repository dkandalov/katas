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
		asBoard(4, Seq()).trim should equal(
"""
X,X,X,X
X,X,X,X
X,X,X,X
X,X,X,X
""".trim)
		asBoard(4, Seq((0, 0), (0, 3), (3, 0), (3, 3))).trim should equal(
"""
Q,X,X,Q
X,X,X,X
X,X,X,X
Q,X,X,Q
""".trim)
	}

	def asBoard(boardSize: Int, solution: Seq[(Int, Int)]): String = {
		val board = for (row <- 0 until boardSize; col <- 0 until boardSize) yield {
			val symbol = if (solution.contains((row, col))) "Q" else "X"
		  val separator = if (col == boardSize - 1) "\n" else ","
			symbol + separator
		}
		board.mkString("")
	}
}