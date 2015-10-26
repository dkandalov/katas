package ru.eightQueen

import org.junit.Test
import org.scalatest.Matchers


class EightQueen13 extends Matchers {
	@Test def `aaa`() {
		queens(0) should equal(List(List()))
		queens(1) should equal(List(List(1)))
		queens(2) should equal(List())
		queens(3) should equal(List())
		queens(4) should equal(List(List(3, 1, 4, 2), List(2, 4, 1, 3)))
		queens(5).size should equal(10)
		queens(8).size should equal(92)
	}

	private def queens(n: Int): List[List[Int]] = {
		def placeQueens(k: Int): List[List[Int]] =
			if (k == 0) List(List())
			else for {
				queens <- placeQueens(k - 1)
				column <- Range(1, n + 1)
				if isSafe(column, queens)
			} yield column :: queens

		placeQueens(n)
	}

	private def isSafe(column: Int, queens: List[Int]): Boolean = {
		val row = 1
		queens.zipWithIndex.forall{ it =>
			val thatColumn = it._1
			val thatRow = it._2 + 2
			column != thatColumn && (row - thatRow).abs != (column - thatColumn).abs
		}
	}
}