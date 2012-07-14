package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 14/07/2012
 */

class EightQueen4 extends ShouldMatchers {
	case class Queen(row: Int, col: Int) {
		def isBefore(queen: Queen) = queen.row > row || (queen.row == row && queen.col >= col)
		def notOnTheSameRowOrColumnAs(queen: Queen) = queen.row != row && queen.col != col
		def notOnTheSameDiagonalAs(queen: Queen) = (queen.row - row).abs != (queen.col - col).abs
	}

	type Solution = Seq[Queen]

	@Test def shouldFindSolutionsForBoardOfSize_4() {
		val solution = solveForBoardOfSize(4)
		asPrintableSolution(solution, 4) should equal(
			"""Vector(X, Q, X, X)
Vector(X, X, X, Q)
Vector(Q, X, X, X)
Vector(X, X, Q, X)

Vector(X, X, Q, X)
Vector(Q, X, X, X)
Vector(X, X, X, Q)
Vector(X, Q, X, X)

"""
		)
	}

	@Test def shouldFindSolutionsForBoardOfSize_8() {
		val solutions = solveForBoardOfSize(8)
		solutions.size should equal(92)
	}

	def solveForBoardOfSize(boardSize: Int): Seq[Solution] = {
		def solve(fromQueen: Queen, solution: Solution): Seq[Solution] = {
			if (solution.size == boardSize) return Seq(solution)

			val result = for (row <- 0 until boardSize; col <- 0 until boardSize) yield {
				val queen = Queen(row, col)
				if (fromQueen.isBefore(queen) && isValidMove(solution, queen))
					solve(queen, solution :+ queen)
				else
					Seq[Solution]()
			}
			result.flatten
		}
		solve(Queen(0, 0), Seq())
	}

	@Test def shouldDetermineIsQueensAreOnTheSameDiagonal() {
		val row = 7
		val col = 5
		Queen(row, col).notOnTheSameDiagonalAs(Queen(row + 2, col - 2)) should be(false) // top-right
		Queen(row, col).notOnTheSameDiagonalAs(Queen(row - 2, col - 2)) should be(false) // top-left
		Queen(row, col).notOnTheSameDiagonalAs(Queen(row - 2, col + 2)) should be(false) // bottom-left
		Queen(row, col).notOnTheSameDiagonalAs(Queen(row + 2, col + 2)) should be(false) // bottom-right
	}

	def isValidMove(solution: Solution, newQueen: Queen): Boolean = {
		solution.forall(_.notOnTheSameRowOrColumnAs(newQueen)) && solution.forall(_.notOnTheSameDiagonalAs(newQueen))
	}

	def asPrintableSolution(solutions: Seq[Solution], boardSize: Int): String = {
		solutions.foldLeft("") { (result, solution) => result + asPrintableBoard(solution, boardSize) + "\n\n" }
	}

	def asPrintableBoard(solution: Seq[Queen], boardSize: Int): String = {
		Range(0, boardSize).map { row =>
			Range(0, boardSize).map { col =>
				if (solution.contains(Queen(row, col))) "Q" else "X"
			}
		}.mkString("\n")
	}
}