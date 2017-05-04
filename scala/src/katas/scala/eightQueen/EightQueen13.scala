package katas.scala.eightQueen

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class EightQueen13 extends ShouldMatchers {
	@Test def `aaa`() {
		queens(0) should equalTo(List(List()))
		queens(1) should equalTo(List(List(1)))
		queens(2) should equalTo(List())
		queens(3) should equalTo(List())
		queens(4) should equalTo(List(List(3, 1, 4, 2), List(2, 4, 1, 3)))
		queens(5).size should equalTo(10)
		queens(8).size should equalTo(92)
	}

	private def queens(boardSize: Int): List[List[Int]] = {
		def placeQueens(rowCount: Int): List[List[Int]] = {
			if (rowCount == 0) List(List())
			else for {
				queens <- placeQueens(rowCount - 1)
				column <- Range.inclusive(1, boardSize)
				if isValidMove(column, queens)
			} yield column :: queens
		}
		placeQueens(boardSize)
	}

	private def isValidMove(column: Int, queenColumns: List[Int]): Boolean = {
		val row = 1
		queenColumns.zipWithIndex.forall{ it =>
			val queenColumn = it._1
			val queenRow = it._2 + 2
			column != queenColumn && (row - queenRow).abs != (column - queenColumn).abs
		}
	}
}