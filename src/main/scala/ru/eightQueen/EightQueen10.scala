package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 09/02/2013
 */

class EightQueen10 extends ShouldMatchers {

	@Test def shouldFindSolutionsForEightQueenProblem() {
		solveForBoardSize(2).size should equal(0)
		solveForBoardSize(3).size should equal(0)
		solveForBoardSize(4).size should equal(2)
		solveForBoardSize(5).size should equal(10)

		// TODO print solutions
	}

	def solveForBoardSize(boardSize: Int, fromCol: Int = 0, solution: Seq[(Int, Int)] = Seq()): Seq[Seq[(Int, Int)]] = {
		if (fromCol == boardSize && solution.size == boardSize) return Seq(solution)

		var result = Seq[Seq[(Int, Int)]]()
		for (col <- Range(fromCol, boardSize)) {
			for (row <- Range(0, boardSize)) {
				if (isValidMove((col, row), solution))
					result = result ++ solveForBoardSize(boardSize, col + 1, solution :+ (col, row))
			}
		}
		result
	}

	def isValidMove(newQueen: (Int, Int), solution: Seq[(Int, Int)]): Boolean = {
		solution.forall{ queen => queen._1 != newQueen._1 && queen._2 != newQueen._2 } &&
		solution.forall{ queen => math.abs(queen._1 - newQueen._1) != math.abs(queen._2 - newQueen._2) }
	}
}