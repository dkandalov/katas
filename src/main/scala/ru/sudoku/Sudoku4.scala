package ru.sudoku

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.collection._
import scala.Some

/**
 * User: dima
 * Date: 14/04/2013
 */

class Sudoku4 extends ShouldMatchers {

	@Test def sudoku() {
		def cross(seq1: Seq[Char], seq2: Seq[Char]): Seq[String] = {
			for (a <- seq1; b <- seq2) yield "" + a + b
		}

		val digits = "123456789"
		val rows = "ABCDEFGHI".toList
		val cols = digits.toList
		val squares = cross(rows, cols)

		val unitlist =
			(for (col <- cols) yield cross(rows, Seq(col))) ++
			(for (row <- rows) yield cross(Seq(row), cols)) ++
			(for (letter <- Seq("ABC", "DEF", "GHI").map{_.toList}; digit <- Seq("123", "456", "789").map{_.toList})
				yield cross(letter, digit))

		val units = (for (square <- squares) yield (square -> unitlist.filter{_.contains(square)})).toMap
		val peers = (for (square <- squares) yield (square -> units(square).flatten.filter{_ != square}.toSet)).toMap

		def test() {
			squares.mkString(", ") should startWith("A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9")
			squares.size should equal(81)
			unitlist.indexWhere{_.mkString(", ") == "A1, B1, C1, D1, E1, F1, G1, H1, I1"} should equal(0)
			unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, A4, A5, A6, A7, A8, A9"} should equal(9)
			unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, B1, B2, B3, C1, C2, C3"} should equal(18)
			unitlist.size should equal(27)
			units("C2").map{"[" + _.mkString(", ") + "]"}.mkString(", ") should
				equal("[A2, B2, C2, D2, E2, F2, G2, H2, I2], [C1, C2, C3, C4, C5, C6, C7, C8, C9], [A1, A2, A3, B1, B2, B3, C1, C2, C3]")
			squares.foreach{ units(_).size should equal(3) }
			peers("C2") should equal(Set(
				"C6", "H2", "C1", "D2", "C8", "C7", "I2", "E2", "C9", "F2",
				"B3", "A2", "C4", "A1", "B2", "C3", "G2", "C5", "A3", "B1"))
			squares.foreach{ peers(_).size should equal(20) }
		}

		def parseGrid(grid: String): Option[mutable.Map[String, String]] = {
			val values = mutable.Map[String, String]((for (s <- squares) yield (s -> digits)) : _*)
			for ((s, d) <- gridValues(grid)) {
				if (digits.contains(d) && assign(values, s, d) == None)
					return None
			}
			Some(values)
		}

		def gridValues(grid: String): Map[String, Char] = {
			val chars = (for (c <- grid; if (digits.contains(c) || "0.".contains(c)))  yield c)
			squares.zip(chars).toMap
		}

		def assign(values: mutable.Map[String, String], s: String, d: Char): Option[mutable.Map[String, String]] = {
			val otherValues = values(s).replace(d.toString, "")
			if ((for (otherDigit <- otherValues) yield eliminate(values, s, otherDigit)).forall{_ != None})
				Some(values)
			else
				None
		}

		def eliminate(values: mutable.Map[String, String], s: String, d: Char): Option[mutable.Map[String, String]] = {
			if (!values(s).contains(d)) return Some(values)
			values(s) = values(s).replace(d.toString, "")

			// (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
			if (values(s).isEmpty)
				return None // Contradiction: removed last value
			else if (values(s).size == 1) {
				val d2 = values(s).head
				if (!(for (s2 <- peers(s)) yield eliminate(values, s2, d2)).forall(_ != None))
					return None
			}

			// (2) If a unit u is reduced to only one place for a value d, then put it there.
			for (u <- units(s)) {
				val dplaces = (for (s <- u; if (values(s).contains(d))) yield s)
				if (dplaces.isEmpty)
					return None // Contradiction: no place for this value
				else if (dplaces.size == 1) {
					// d can only be in one place in unit; assign it there
					if (assign(values, dplaces(0), d) == None)
						return None
				}
			}

			Some(values)
		}

		def display(values: mutable.Map[String, String]) {
			val width = 1 + (for (s <- squares) yield values(s).size).max
			val line = Seq.fill(3){ Seq.fill(width * 3){'-'}.mkString("") }.mkString("+")
			for (r <- rows) {
				println((for (c <- cols) yield values(r.toString + c).formatted("%" + width + "s") + (if ("36".contains(c)) "|" else "")).mkString(""))
				if ("CF".contains(r)) println(line)
			}
		}

		def solve(grid: String): Option[mutable.Map[String, String]] = {
			search(parseGrid(grid))
		}

		def search(values: Option[mutable.Map[String, String]]): Option[mutable.Map[String, String]] = {
			if (values == None)
				return None // Failed earlier
			if ((for (s <- squares) yield values.get(s)).forall{_.size == 1})
				return values // Solved!

			// Chose the unfilled square s with the fewest possibilities
			val (n, s) = (for (s <- squares; if (values.get(s).size > 1)) yield (values.get(s).size, s)).minBy{_._1}
			val result = (for (d <- values.get(s)) yield search(assign(values.get.clone(), s, d))).find{_ != None}
			if (result == None) None else result.get
		}

		test()
		println(parseGrid("""
4 . . |. . . |8 . 5
. 3 . |. . . |. . .
. . . |7 . . |. . .
------+------+------
. 2 . |. . . |. 6 .
. . . |. 8 . |4 . .
. . . |. 1 . |. . .
------+------+------
. . . |6 . 3 |. 7 .
5 . . |2 . . |. . .
1 . 4 |. . . |. . .
"""))

		val easy_puzzle_grid = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
		display(parseGrid(easy_puzzle_grid).get)

		val hard_puzzle_grid = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
		display(parseGrid(hard_puzzle_grid).get)
		display(solve(hard_puzzle_grid).get)
	}
}