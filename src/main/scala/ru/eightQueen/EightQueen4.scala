package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 14/07/2012
 */

class EightQueen4 extends ShouldMatchers {
	type Queen = (Int, Int)
	type Solution = Seq[Queen]

	@Test def shouldFindSolutionForBoardOfSize_4() {
		val solution = solveForBoardOfSize(4)
		println(asPrintableSolution(solution, 4))
	}

	def solveForBoardOfSize(size: Int): Seq[Solution] = {
		def solve(solution: Solution): Seq[Solution] = {
			if (solution.size == size) return Seq(solution)

			var result = Seq[Solution]()
			Range(0, size).foreach { row =>
				Range(0, size).foreach { col =>
					val queen = (row, col)
					if (correctMove(solution, queen))
						result = result ++ solve(solution :+ queen)
				}
			}
			result
		}
		solve(Seq())
	}

	@Test def correctMove() {
		correctMove(Seq(), (0, 0)) should be(true)

		correctMove(Seq((0, 0)), (1, 2)) should be(true)

		correctMove(Seq((0, 0)), (0, 0)) should be(false)
		correctMove(Seq((0, 0)), (1, 0)) should be(false)
		correctMove(Seq((0, 0)), (0, 1)) should be(false)

		correctMove(Seq((0, 0)), (1, 1)) should be(false)
		correctMove(Seq((0, 0)), (2, 2)) should be(false)
		correctMove(Seq((0, 2)), (2, 0)) should be(false)
	}

	def correctMove(solution: Solution, newQueen: Queen): Boolean = {
		def notOnTheSameRowOrColumn = solution.forall { queen => queen._1 != newQueen._1 && queen._2 != newQueen._2 }
		def notOnTheSameDiagonal = solution.forall{ queen => (queen._1 - queen._2).abs != (newQueen._1 - newQueen._2).abs }
		notOnTheSameRowOrColumn && notOnTheSameDiagonal
	}

	def asPrintableSolution(solutions: Seq[Solution], boardSize: Int): String = {
		solutions.foldLeft("") { (result, solution) => result + asPrintableBoard(solution, boardSize) + "\n\n" }
	}

	def asPrintableBoard(solution: Seq[Queen], boardSize: Int): String = {
		Range(0, boardSize).map { row =>
			Range(0, boardSize).map { col =>
				if (solution.contains((row, col))) "Q" else "X"
			}
		}.mkString("\n")
	}
}