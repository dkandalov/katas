package ru.sudoku

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 14/04/2013
 */

class Sudoku4 extends ShouldMatchers {
	@Test def sudoku() {
		def cross(seq1: Seq[Char], seq2: Seq[Char]): Seq[String] = {
			for (a <- seq1; b <- seq2) yield "" + a + b
		}

		val digits = "123456789".toList
		val rows = "ABCDEFGHI".toList
		val cols = digits
		val squares = cross(rows, cols)
		squares.mkString(", ") should startWith("A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9")

		val unitlist =
			(for (col <- cols) yield cross(rows, Seq(col))) ++
			(for (row <- rows) yield cross(Seq(row), cols)) ++
			(for (letter <- Seq("ABC", "DEF", "GHI").map{_.toList}; digit <- Seq("123", "456", "789").map{_.toList})
				yield cross(letter, digit))
		unitlist.indexWhere{_.mkString(", ") == "A1, B1, C1, D1, E1, F1, G1, H1, I1"} should equal(0)
		unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, A4, A5, A6, A7, A8, A9"} should equal(9)
		unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, B1, B2, B3, C1, C2, C3"} should equal(18)

		val units = (for (square <- squares) yield (square -> unitlist.filter{_.contains(square)})).toMap
		units("A1").map{"[" + _.mkString(", ") + "]"}.mkString(", ") should
			equal("[A1, B1, C1, D1, E1, F1, G1, H1, I1], [A1, A2, A3, A4, A5, A6, A7, A8, A9], [A1, A2, A3, B1, B2, B3, C1, C2, C3]")

//		val peers = TODO
	}
}