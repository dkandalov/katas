package ru.eightQueen

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 23/07/2012
 */

class EightQueen7 extends ShouldMatchers {
	@Test def shouldSolveForBoardOfSize_4() {
		val solutions = solveForBoard(4)
		solutions.foreach { solution => println(asBoard(4, solution)) }
		solutions.foreach { solution =>
			solution.size should equal(4)
			solution.foreach { queen =>
				val noQueensOnSameRowOrColumn = solution.filter(_ != queen).forall{ otherQueen => otherQueen._1 != queen._1 && otherQueen._2 != queen._2 }
				val noQueensOnSameDiagonal = solution.filter(_ != queen).forall{ otherQueen => (otherQueen._1 - queen._2).abs != (otherQueen._2 - queen._2).abs }
				noQueensOnSameRowOrColumn should equal(true)
			}
		}
//		solutions.size should equal(5)
	}

	def solveForBoard(boardSize: Int): Seq[Seq[(Int, Int)]] = {
		Seq(Seq((1,1), (2,2), (2,2), (2,2)))
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