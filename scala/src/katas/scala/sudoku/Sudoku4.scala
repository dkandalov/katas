package katas.scala.sudoku

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.collection._

/**
 * User: dima
 * Date: 14/04/2013
 */

class Sudoku4 extends ShouldMatchers {

	@Test def `should parse sudoku puzzles`() {
		parseGrid("""
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
""").toString should equalTo("Some(Map(F9 -> 23789, E3 -> 15679, F4 -> 359, I1 -> 1, I5 -> 579, C1 -> 2689, D2 -> 2, D7 -> 13579, B3 -> 1256789, C5 -> 234569, G8 -> 7, G3 -> 289, G7 -> 1259, F6 -> 25679, F1 -> 36789, G4 -> 6, F7 -> 23579, C2 -> 15689, C6 -> 245689, B5 -> 24569, E6 -> 25679, C4 -> 7, C9 -> 123469, D5 -> 34579, H3 -> 3, D9 -> 13789, F2 -> 4, I8 -> 23589, H7 -> 69, H2 -> 6789, G6 -> 3, G1 -> 289, G5 -> 459, A4 -> 139, A9 -> 5, A5 -> 2369, A6 -> 269, D1 -> 3789, D6 -> 4579, E8 -> 12359, I6 -> 5789, H8 -> 489, G2 -> 89, H9 -> 4689, H4 -> 2, I7 -> 23569, I2 -> 6789, H1 -> 5, H6 -> 1, H5 -> 479, B9 -> 124679, A3 -> 12679, B4 -> 14589, A8 -> 1239, B8 -> 1249, A2 -> 1679, A7 -> 8, D4 -> 3459, E5 -> 8, D3 -> 15789, E4 -> 359, E9 -> 12379, D8 -> 6, E1 -> 3679, G9 -> 12489, F3 -> 56789, F8 -> 23589, E2 -> 15679, E7 -> 4, I3 -> 4, I4 -> 589, I9 -> 23689, C7 -> 12369, B6 -> 245689, B1 -> 26789, C3 -> 125689, B2 -> 3, C8 -> 12349, B7 -> 12679, A1 -> 4, F5 -> 1))")
	}

	@Test def `should solve easy puzzle`() {
		val easy_puzzle_grid = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
		display(parseGrid(easy_puzzle_grid).get)
	}

	@Test def `should solve hard puzzle`() {
		val hard_puzzle_grid = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
		display(parseGrid(hard_puzzle_grid).get)
		display(solve(hard_puzzle_grid).get)
	}

	def cross(seq1: Seq[Char], seq2: Seq[Char]): Seq[String] = {
		for (a <- seq1; b <- seq2) yield "" + a + b
	}

	@Test def `calculation of field values`() {
		squares.mkString(", ") should startWith("A1, A2, A3, A4, A5, A6, A7, A8, A9, B1, B2, B3, B4, B5, B6, B7, B8, B9")
		squares.size should equalTo(81)

		unitlist.indexWhere{_.mkString(", ") == "A1, B1, C1, D1, E1, F1, G1, H1, I1"} should equalTo(0)
		unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, A4, A5, A6, A7, A8, A9"} should equalTo(9)
		unitlist.indexWhere{_.mkString(", ") == "A1, A2, A3, B1, B2, B3, C1, C2, C3"} should equalTo(18)
		unitlist.size should equalTo(27)

		units("C2").map{"[" + _.mkString(", ") + "]"}.mkString(", ") should
			equalTo("[A2, B2, C2, D2, E2, F2, G2, H2, I2], [C1, C2, C3, C4, C5, C6, C7, C8, C9], [A1, A2, A3, B1, B2, B3, C1, C2, C3]")
		squares.foreach{ units(_).size should equalTo(3) }

		peers("C2") should equalTo(Set(
			"C6", "H2", "C1", "D2", "C8", "C7", "I2", "E2", "C9", "F2",
			"B3", "A2", "C4", "A1", "B2", "C3", "G2", "C5", "A3", "B1"))
		squares.foreach{ peers(_).size should equalTo(20) }
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

}