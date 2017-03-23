package katas.scala.eightQueen

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 09/02/2013
 */

class EightQueen10 extends Matchers {

	@Test def shouldFindSolutionsForEightQueenProblem() {
		solveForBoardSize(2).size should equal(0)
		solveForBoardSize(3).size should equal(0)
		solveForBoardSize(4).size should equal(2)
		solveForBoardSize(5).size should equal(10)
		solveForBoardSize(8).size should equal(92)
	}

	@Test def printingSolutions() {
		solveForBoardSize(4).map{ asBoard }.mkString("\n=====\n") should equal("""
		  |-Q--
		  |---Q
		  |Q---
		  |--Q-
 		  |=====
		  |--Q-
		  |Q---
		  |---Q
		  |-Q--
		""".trim.stripMargin)
	}

	private def asBoard(solution: Seq[(Int, Int)]): String = {
		val boardSize = math.max(solution.maxBy{_._1}._1, solution.maxBy{_._2}._2) + 1
		(for (col <- Range(0, boardSize))
			yield (for (row <- Range(0, boardSize))
				yield if (solution.contains((col, row))) "Q" else "-").mkString("")
		).mkString("\n")
	}

	private def solveForBoardSize(boardSize: Int, fromCol: Int = 0, solution: Seq[(Int, Int)] = Seq()): Seq[Seq[(Int, Int)]] = {
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

	private def isValidMove(newQueen: (Int, Int), solution: Seq[(Int, Int)]): Boolean = {
		solution.forall{ queen => queen._1 != newQueen._1 && queen._2 != newQueen._2 } &&
		solution.forall{ queen => math.abs(queen._1 - newQueen._1) != math.abs(queen._2 - newQueen._2) }
	}
}