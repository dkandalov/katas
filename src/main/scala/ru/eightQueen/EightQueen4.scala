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
		def solve(solution: Solution): Seq[Solution] = {
			var result = Seq[Solution]()
			Range(0, size).foreach { row =>
				Range(0, size).foreach { col =>
					val move = (row, col)
					if (correctMove(solution, move))
						result = result ++ solve(solution :+ move)
				}
			}
			result
		}
		solve(Seq())
	}

	def correctMove(solution: Solution, tuple: Position): Boolean = {
		false
	}

	def asPrintableSolution(solutions: Seq[Solution], boardSize: Int): String = {
		solutions.foldLeft("") { (result, solution) => result + asPrintableBoard(solution, boardSize) + "\n\n" }
	}

	def asPrintableBoard(solution: Seq[Position], boardSize: Int): String = {
		Range(0, boardSize).map { row =>
			Range(0, boardSize).map { col =>
				if (solution.contains((row, col))) "0" else "X"
			}
		}.mkString("\n")
	}
}