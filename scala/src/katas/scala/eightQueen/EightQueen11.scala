package katas.scala.eightQueen

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class EightQueen11 extends ShouldMatchers {
	@Test def findAllValidQueenPositions() {
		solveForBoardOfSize(3) should equalTo(Seq())
		solveForBoardOfSize(4).map{ solution => asPrintableBoard(4, solution) }.mkString("\n\n") should equalTo(
			"""
			  |--Q-
			  |Q---
			  |---Q
			  |-Q--
			  |
			  |-Q--
			  |---Q
			  |Q---
			  |--Q-
			""".stripMargin.trim)
		solveForBoardOfSize(5).map{ solution => asPrintableBoard(5, solution) }.mkString("\n\n") should equalTo(
			"""
			  |Q----
			  |---Q-
			  |-Q---
			  |----Q
			  |--Q--
			  |
			  |Q----
			  |--Q--
			  |----Q
			  |-Q---
			  |---Q-
			  |
			  |--Q--
			  |Q----
			  |---Q-
			  |-Q---
			  |----Q
			  |
			  |---Q-
			  |Q----
			  |--Q--
			  |----Q
			  |-Q---
			  |
			  |-Q---
			  |---Q-
			  |Q----
			  |--Q--
			  |----Q
			  |
			  |----Q
			  |--Q--
			  |Q----
			  |---Q-
			  |-Q---
			  |
			  |-Q---
			  |----Q
			  |--Q--
			  |Q----
			  |---Q-
			  |
			  |----Q
			  |-Q---
			  |---Q-
			  |Q----
			  |--Q--
			  |
			  |---Q-
			  |-Q---
			  |----Q
			  |--Q--
			  |Q----
			  |
			  |--Q--
			  |----Q
			  |-Q---
			  |---Q-
			  |Q----
			""".stripMargin.trim
		)

		solveForBoardOfSize(8).size should equalTo(92)
	}

	def solveForBoardOfSize(boardSize: Int, fromColumn: Int = 0, solution: Seq[(Int, Int)] = Seq()): Seq[Seq[(Int, Int)]] = {
		var solutions = Seq[Seq[(Int, Int)]]()
		(fromColumn until boardSize).foreach{ column =>
			(0 until boardSize).foreach { row =>
				val queen = (row, column)
				if (isValidSolution(solution :+ queen)) {
					solutions = solutions ++ solveForBoardOfSize(boardSize, column + 1, solution :+ queen)
				}
			}
		}
		if (solution.size == boardSize) solutions :+ solution else solutions
	}

	def isValidSolution(solution: Seq[(Int, Int)]): Boolean = {
		val notOnTheSameLine = solution.forall{ queen =>
			solution.filterNot{_ == queen}.forall{ it => it._1 != queen._1 && it._2 != queen._2 }
		}
		val notOnTheSameDiagonal = solution.forall{ queen =>
			solution.filterNot{_ == queen}.forall{ it => math.abs(it._1 - queen._1) != math.abs(it._2 - queen._2) }
		}
		notOnTheSameLine && notOnTheSameDiagonal
	}

	@Test def convertSolutionToPrintableString() {
		asPrintableBoard(3, Seq()) should equalTo("""
			  |---
			  |---
			  |---
			""".stripMargin.trim)
		asPrintableBoard(3, Seq((0, 0))) should equalTo("""
			  |Q--
			  |---
			  |---
			""".stripMargin.trim)
		asPrintableBoard(3, Seq((2, 2))) should equalTo("""
			  |---
			  |---
			  |--Q
			""".stripMargin.trim)
		asPrintableBoard(3, Seq((1, 1))) should equalTo("""
			  |---
			  |-Q-
			  |---
			""".stripMargin.trim)
	}

	def asPrintableBoard(boardSize: Int, queenPositions: Seq[(Int, Int)]): String = {
		(0 until boardSize).map{ row =>
			(0 until boardSize).map{ column =>
				if (queenPositions.contains((row, column))) "Q" else "-"
			}.mkString("")
		}.mkString("\n")
	}
}