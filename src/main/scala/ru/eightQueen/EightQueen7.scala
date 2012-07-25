package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 23/07/2012
 */

class EightQueen7 extends ShouldMatchers {
	@Test def shouldSolveForBoardOfSize_4() {
		val boardSize = 4

		val solutions = solveForBoard(boardSize)
		println(solutions)
		assertIsValid(solutions, boardSize)

		solutions.size should equal(2)
	}

	@Test def shouldSolveForBoardOfSize_5() {
		val boardSize = 5

		val solutions = solveForBoard(boardSize)
		println(solutions)
		assertIsValid(solutions, boardSize)

		solutions.size should equal(10)
	}

	@Test def shouldSolveForBoardOfSize_8() {
		val boardSize = 8

		val solutions = solveForBoard(boardSize)
		println(solutions)
		assertIsValid(solutions, boardSize)

		solutions.size should equal(92)
	}

	def assertIsValid(solutions: scala.Seq[scala.Seq[(Int, Int)]], boardSize: Int) {
		solutions.foreach { solution => println(asBoard(boardSize, solution))}
		solutions.foreach { _.size should equal(boardSize)}
		solutions.foreach { solution =>
				solution.foreach { queen =>
						noQueensOnSameRowOrColumn(solution.filterNot(_ == queen), queen) should equal(true)
						noQueensOnSameDiagonal(solution.filterNot(_ == queen), queen) should equal(true)
				}
		}
	}

	def solveForBoard(boardSize: Int): Seq[Seq[(Int, Int)]] = {
		val initialPosition = (-1, -1)
		val emptySolution = Seq()
		doSolve(initialPosition, emptySolution, boardSize)
	}

	def doSolve(fromQueen: (Int, Int), solution: Seq[(Int, Int)], boardSize: Int): Seq[Seq[(Int, Int)]] = {
		if (solution.size == boardSize) return Seq(solution)

		val result = for (row <- 0 until boardSize; col <- 0 until boardSize) yield {
			if (row > fromQueen._1 || (row == fromQueen._1 && col > fromQueen._2)) {
				val newQueen = (row, col)
				if (noQueensOnSameRowOrColumn(solution, newQueen) && noQueensOnSameDiagonal(solution, newQueen)) {
					doSolve(newQueen, solution :+ newQueen, boardSize)
				} else {
					Seq()
				}
			} else {
				Seq()
			}
		}
		result.flatten
	}

	def noQueensOnSameRowOrColumn(solution: Seq[(Int, Int)], queen: (Int, Int)) =
		solution.forall{ otherQueen => otherQueen._1 != queen._1 && otherQueen._2 != queen._2 }

	def noQueensOnSameDiagonal(solution: Seq[(Int, Int)], queen: (Int, Int)) =
		solution.forall{ otherQueen => (otherQueen._1 - queen._1).abs != (otherQueen._2 - queen._2).abs }

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