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
		squares.size should equal(81)

		val unitlist =
			(for (col <- cols) yield cross(rows, Seq(col))) ++
			(for (row <- rows) yield cross(Seq(row), cols)) ++
			(for (letter <- Seq("ABC", "DEF", "GHI").map{_.toList}; digit <- Seq("123", "456", "789").map{_.toList})
				yield cross(letter, digit))
		unitlist.indexWhere{_.mkString(", ") == "A1, B1, C1, D1, E1, F1, G1, H1, I1"} should equal(0)
		unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, A4, A5, A6, A7, A8, A9"} should equal(9)
		unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, B1, B2, B3, C1, C2, C3"} should equal(18)
		unitlist.size should equal(27)

		val units = (for (square <- squares) yield (square -> unitlist.filter{_.contains(square)})).toMap
		units("C2").map{"[" + _.mkString(", ") + "]"}.mkString(", ") should
			equal("[A2, B2, C2, D2, E2, F2, G2, H2, I2], [C1, C2, C3, C4, C5, C6, C7, C8, C9], [A1, A2, A3, B1, B2, B3, C1, C2, C3]")
		squares.foreach{ units(_).size should equal(3) }

		val peers = (for (square <- squares) yield (square -> units(square).flatten.filter{_ != square}.toSet)).toMap
		peers("C2") should equal(Set(
			"C6", "H2", "C1", "D2", "C8", "C7", "I2", "E2", "C9", "F2",
			"B3", "A2", "C4", "A1", "B2", "C3", "G2", "C5", "A3", "B1"))
		squares.foreach{ peers(_).size should equal(20) }


		def parseGrid(grid: String): Option[Map[String, String]] = {
			var values: Option[Map[String, String]] = Some((for (square <- squares) yield (square -> digits.mkString(""))).toMap)
			for (entry <- gridValues(grid)) {
				assign(values, entry._1, entry._2) match {
					case Some(x) => values = Some(x)
					case None => return None
				}
			}
			values
		}

		def assign(values: Option[Map[String, String]], square: String, digit: Char): Option[Map[String, String]] = {
			None // TODO
		}

		def gridValues(grid: String): Map[String, Char] = {
			val chars = (for (char <- grid; if (digits.contains(char) || "0.".contains(char)))  yield char)
			squares.zip(chars).toMap
		}
	}
}