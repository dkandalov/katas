package ru._99_problems.original

import org.junit.Test
import org.scalatest.Matchers
import ru._99_problems.original.SudokuBoard.{solve, string2Board}


class SudokuBoardTest extends Matchers {
	@Test def `solving sudoku examples`(): Unit = {
		var board: SudokuBoard[Int] = null

		board = """
			|..48...17
			|67.9.....
			|5.8.3...4
			|3..74.1..
			|.69...78.
			|..1.69..5
			|1...8.3.6
			|.....6.91
			|24...15..
    """.stripMargin.trim.replace("\n", "")

		solve(board).get.toString should equal("""
       |9  3  4 | 8  2  5 | 6  1  7
       |6  7  2 | 9  1  4 | 8  5  3
       |5  1  8 | 6  3  7 | 9  2  4
       |--------+---------+--------
       |3  2  5 | 7  4  8 | 1  6  9
       |4  6  9 | 1  5  3 | 7  8  2
       |7  8  1 | 2  6  9 | 4  3  5
       |--------+---------+--------
       |1  9  7 | 5  8  2 | 3  4  6
       |8  5  3 | 4  7  6 | 2  9  1
       |2  4  6 | 3  9  1 | 5  7  8
     """.stripMargin.trim)


		// medium
		// http://dailysudoku.com/sudoku/archive/2016/11/2016-11-6_solution.shtml
		var s = """
	    |6.....2.3
	    |...4.38..
	    |.3.7....9
	    |....2.1..
	    |49.....65
	    |..6.9....
	    |1....5.8.
	    |..96.....
	    |8.4.....2
    """.stripMargin.trim.replace("\n", "")
		solve(string2Board(s)).get.toString should equal("""
      |6  4  1 | 8  5  9 | 2  7  3
      |9  2  7 | 4  1  3 | 8  5  6
      |5  3  8 | 7  6  2 | 4  1  9
      |--------+---------+--------
      |7  8  5 | 3  2  6 | 1  9  4
      |4  9  2 | 1  8  7 | 3  6  5
      |3  1  6 | 5  9  4 | 7  2  8
      |--------+---------+--------
      |1  6  3 | 2  4  5 | 9  8  7
      |2  7  9 | 6  3  8 | 5  4  1
      |8  5  4 | 9  7  1 | 6  3  2
    """.stripMargin.trim)


		// hard
		// http://dailysudoku.com/sudoku/archive/2016/11/2016-11-5_solution.shtml
		s = """
	    |.7...1...
	    |19.6.5...
	    |84..7..9.
	    |9....85..
	    |5.7...8.1
	    |..15....2
	    |.5..2..19
	    |...9.4.86
	    |...1...5.
    """.stripMargin.trim.replace("\n", "")
		solve(string2Board(s)).get.toString should equal("""
      |2  7  5 | 8  9  1 | 6  3  4
      |1  9  3 | 6  4  5 | 7  2  8
      |8  4  6 | 3  7  2 | 1  9  5
      |--------+---------+--------
      |9  6  4 | 2  1  8 | 5  7  3
      |5  2  7 | 4  3  9 | 8  6  1
      |3  8  1 | 5  6  7 | 9  4  2
      |--------+---------+--------
      |6  5  8 | 7  2  3 | 4  1  9
      |7  1  2 | 9  5  4 | 3  8  6
      |4  3  9 | 1  8  6 | 2  5  7
    """.stripMargin.trim)


		// very hard
		// http://dailysudoku.com/sudoku/archive/2016/11/2016-11-7_solution.shtml
		s = """
	    |.1......4
	    |7.9..1..5
	    |..59.7..6
	    |3.4....2.
	    |...3.2...
	    |.7....9.3
	    |9..8.64..
	    |2..5..3.1
	    |1......6.
    """.stripMargin.trim.replace("\n", "")
		solve(string2Board(s)).get.toString should equal("""
	     |8  1  6 | 2  3  5 | 7  9  4
	     |7  2  9 | 4  6  1 | 8  3  5
	     |4  3  5 | 9  8  7 | 2  1  6
	     |--------+---------+--------
	     |3  8  4 | 6  5  9 | 1  2  7
	     |5  9  1 | 3  7  2 | 6  4  8
	     |6  7  2 | 1  4  8 | 9  5  3
	     |--------+---------+--------
	     |9  5  3 | 8  1  6 | 4  7  2
	     |2  6  7 | 5  9  4 | 3  8  1
	     |1  4  8 | 7  2  3 | 5  6  9
	  """.stripMargin.trim)

	}
}