package ru._99_problems

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class Misc extends ShouldMatchers {
	@Test def `P90 (**) Eight queens problem`() {
		solveEightQueen(2) should equal(Seq())
		solveEightQueen(3) should equal(Seq())
		solveEightQueen(4).size should equal(2)
		solveEightQueen(5).size should equal(10)
		solveEightQueen(8).size should equal(92)
	}


	private case class Queen(col: Int, row: Int) {
		def attacks(that: Queen): Boolean = {
			col == that.col || row == that.row || (col - that.col).abs == (row - that.row).abs
		}
	}
	private type Solution = Seq[Queen]

	private def solveEightQueen(boardSize: Int, startCol: Int = 0): Seq[Solution] = {
		if (boardSize < 3) return Seq()
		if (startCol >= boardSize) return Seq(Seq())

		val solutions = solveEightQueen(boardSize, startCol + 1)
		Range(0, boardSize).flatMap{ row =>
			val queen = Queen(startCol, row)
			solutions.filter(isValidWith(queen, _)).map(queen +: _)
		}
	}

	private def isValidWith(queen: Queen, solution: Solution) = solution.forall(!queen.attacks(_))

	private def asBoard(solution: Solution, boardSize: Int): String = {
		val result = Array.fill[Char](boardSize, boardSize){ '-' }
		solution.foreach{ queen => result(queen.row)(queen.col) = 'Q' }
		result.map{ row => row.mkString("") }.mkString("\n")
	}

	@Test def `presenting solution as a board`() {
		asBoard(Seq(Queen(0,0), Queen(2, 2)), 3) should equal(
			"""
			  |Q--
			  |---
			  |--Q
			""".trim.stripMargin)
	}

	@Test def `amount of queens in solution should equal board size`() {
		val solutions = solveEightQueen(8)
		solutions.size should be(92)
		solutions.foreach{ solution =>
			solution.length should equal(8)
		}
	}

	@Test def `queen is on the same row, column or diagonal`() {
		Queen(0, 0).attacks(Queen(0, 2)) should be(true)
		Queen(0, 0).attacks(Queen(2, 0)) should be(true)
		Queen(0, 0).attacks(Queen(2, 2)) should be(true)
		Queen(0, 0).attacks(Queen(2, 1)) should be(false)
	}
}