package ru.sudoku

import org.junit.Test
import ru.util.GroovyUtil

/**
 * User: dima
 * Date: 28/12/2011
 */
class Sudoku3 {
  @Test public void shouldSolveSimpleSudoku() {
    def sudoku = """
             0, 0, 6,  8, 0, 0,  3, 9, 0,
             0, 2, 0,  0, 5, 0,  0, 4, 6,
             9, 0, 0,  6, 0, 2,  0, 0, 0,

             0, 0, 0,  0, 7, 0,  4, 8, 0,
             0, 9, 7,  0, 8, 0,  6, 5, 0,
             0, 5, 1,  0, 4, 0,  0, 0, 0,

             0, 0, 0,  1, 0, 5,  0, 0, 4,
             2, 6, 0,  0, 9, 0,  0, 1, 0,
             0, 1, 5,  0, 0, 8,  7, 0, 0
"""
    SudokuSolver.parse(sudoku).with {
      println boardAsString
      println solve()
      println boardAsString
    }
  }

  @Test public void shouldSolveMoreComplexSudoku() { // TODO
    def sudoku = """
             0, 8, 0,  0, 0, 0,  3, 0, 0,
             2, 0, 0,  0, 0, 0,  6, 0, 0,
             9, 0, 0,  0, 1, 0,  0, 0, 2,

             0, 6, 0,  0, 8, 0,  0, 4, 0,
             0, 0, 0,  0, 0, 0,  0, 0, 5,
             0, 0, 0,  7, 2, 9,  0, 6, 0,

             0, 0, 5,  0, 0, 2,  1, 7, 0,
             7, 4, 0,  0, 0, 0,  0, 0, 0,
             0, 0, 8,  0, 3, 0,  0, 0, 9,
"""
    SudokuSolver.parse(sudoku).with {
      println boardAsString
      println solve()
      println boardAsString
    }
  }

  static class SudokuSolver {
    def board
    def boardOfHints

    static SudokuSolver parse(String s) {
      def board = s.replaceAll(/[\s\n]/, "").split(/,/).collect {Integer.parseInt(it)}
      new SudokuSolver(board)
    }

    SudokuSolver(board) {
      this.board = board
      this.boardOfHints = createHints()
    }

    def createHints() {
      def result = []
      allCellsByRow().each { cell ->
        if (valueAt(cell) == 0) {
          result << (1..9).findAll { canBeUsedAt(cell, it) }
        } else {
          result << []
        }
      }
      // TODO number claiming, pairs, triples, N-sets
      result
    }

    def solve() {
      while (!solved) {
        def step = [{ singleCandidateCell() }, { singleCellCandidate() }].findResult { it.call() }
        if (step != null) {
          println step
          setValueAt(step.cell, step.value)
          boardOfHints = createHints()
        } else {
          println "no more moves :("
          return false
        }
      }
      return true
    }

    def singleCellCandidate() {
      def result = (1..9).findResult { value ->
        def resultCell = (0..8).findResult { row -> cellsInRow(row).findOne { cell -> hintsAt(cell).contains(value) } }
        if (resultCell != null) [value, resultCell]

        resultCell = (0..8).findResult { column -> cellsInColumn(column).findOne { cell -> hintsAt(cell).contains(value) } }
        if (resultCell != null) [value, resultCell]

        resultCell = (0..2).findResult { sqRow ->
          (0..2).findResult { sqCol ->
            cellsInSquare(sqRow, sqCol).findOne { cell -> hintsAt(cell).contains(value) }
          }
        }
        if (resultCell != null) [value, resultCell] else null
      }
      if (result == null) return null

      [value: result[0], cell: result[1]]
    }

    def singleCandidateCell() {
      def cell = allCellsByRow().find { hintsAt(it).size() == 1 }
      if (cell == null) return null

      [value: hintsAt(cell)[0], cell: cell]
    }

    def hintsAt(cell) {
      boardOfHints[cell.row * 9 + cell.column]
    }

    boolean canBeUsedAt(cell, value) {
      canBeUsedInRow(cell.row, value) && canBeUsedInColumn(cell.column, value) && canBeUsedInSquare(cell.row, cell.column, value)
    }

    boolean canBeUsedInSquare(row, column, value) {
      cellsInSquare(row.intdiv(3), column.intdiv(3)).every { valueAt(it) != value }
    }

    boolean canBeUsedInRow(row, value) {
      cellsInRow(row).every { valueAt(it) != value }
    }

    boolean canBeUsedInColumn(col, value) {
      // was cellsInRow instead of cellsInColumn :(
      cellsInColumn(col).every { valueAt(it) != value }
    }

    def cellsInSquare(rowOfSquares, columnOfSquares) {
      // didn't multiply by 3
      (rowOfSquares * 3..rowOfSquares * 3 + 2).collect { row ->
        (columnOfSquares * 3..columnOfSquares * 3 + 2).collect { column -> [row: row, column: column] }
      }.flatten()
    }

    def allCellsByRow() {
      (0..8).collect { row -> cellsInRow(row) }.flatten()
    }

    def cellsInRow(row) {
      (0..8).collect { [row: row, column: it] }
    }

    def cellsInColumn(column) {
      (0..8).collect { [row: it, column: column] }
    }

    def setValueAt(cell, value) {
      board[cell.row * 9 + cell.column] = value
    }

    def valueAt(cell) {
      board[cell.row * 9 + cell.column]
    }

    boolean isSolved() {
      !board.contains(0)
    }

    String getBoardAsString() {
      board.injectWithIndex("") { result, value, i ->
        if (i != 0) {
          if (i % 27 == 0) result += "\n\n"
          else if (i % 9 == 0) result += "\n"
          else if (i % 3 == 0) result += " "
        }
        result + value + ", "
      }
    }

    static {
      Collection.metaClass.mixin(GroovyUtil)
    }
  }
}
