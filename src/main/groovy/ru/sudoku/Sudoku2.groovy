package ru.sudoku

import org.junit.Test
import ru.util.GroovyUtil

/**
 * User: dima
 * Date: 26/12/2011
 */
class Sudoku2 {
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
    new SudokuSolver(sudoku).with {
      println toReadableString()
      solve()
    }

  }

  @Test public void shouldSolveSudoku() {
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
    def sudokuSolver = new SudokuSolver(sudoku)
  }

  private static class SudokuSolver {
    def data

    SudokuSolver(String sudoku) {
      data = sudoku.replaceAll(/[\s\n]/, "").split(/,/).collect {Integer.parseInt(it)}
    }

    void solve() {
      int numberOfSteps = 0
      while (!isSolved() && numberOfSteps < 9 * 9) {
        def lastStep = makeStep()
        break
        println lastStep
        numberOfSteps++
      }
    }

    def makeStep() {
      def step

      step = (2).findResult { value ->
        // TODO: fix it
        // used the whole board for iteration instead of just one row :(
        def position = emptyCellsByRow().findOne { cell -> canBeUsedAt(value, cell.row, cell.column) } // used "row, column ->" instead of one param
        println position
        if (position != null) return [value, position]

        position = emptyCellsByColumn().findOne { cell -> canBeUsedAt(value, cell.row, cell.column) }
        if (position != null) return [value, position] else return null
      }

      if (step != null) {
        def (row, col) = step[1]
        setValueAt(row, col, step[0])
      }
      step
    }

    def canBeUsedAt(value, row, column) {
      def r = canBeUsedInSquare(value, row, column) && canBeUsedInColumn(value, column) && canBeUsedInRow(value, row)
      println "$row - $column = ${canBeUsedInRow(value, column) }"
      println "$row - $column = ${canBeUsedInColumn(value, column) }"
      println "$row - $column = ${canBeUsedInSquare(value, row, column) }"
      r
    }

    boolean canBeUsedInColumn(value, column) {
      (0..8).every { row -> valueAt(row, column) != value }
    }

    boolean canBeUsedInRow(value, row) {
      (0..8).every { col -> valueAt(row, col) != value }
    }

    boolean canBeUsedInSquare(value, row, column) {
      cellsInSquare(row.intdiv(3), column.intdiv(3)).every { cell -> valueAt(cell.row, cell.column) != value } // compared with 0 instead of "value"
    }

    def cellsInSquare(squareRow, squareCol) {
      def result = []
      (squareRow..squareRow + 2).each { row ->
        (squareCol..squareCol + 2).each { col ->
          result << [row: row, column: col]
        }
      }
      result
    }

    def emptyCellsByRow() {
      def result = []
      for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
          if (valueAt(row, col) == 0) result << [row: row, column: col]
        }
      }
      result
    }

    def emptyCellsByColumn() {
      def result = []
      for (int col = 0; col < 9; col++) {
        for (int row = 0; row < 9; row++) {
          if (valueAt(row, col) == 0) result << [row: row, column: col]
        }
      }
      result
    }

    def valueAt(row, column) {
      data[row * 9 + column]
    }

    def setValueAt(row, column, value) {
      data[row * 9 + column] = value
    }

    boolean isSolved() {
      !data.contains(0)
    }

    def toReadableString() {
      data.injectWithIndex(new StringBuilder()) { result, cell, i ->
        if (i > 0) {
          if (i % 27 == 0) {
            result.append("\n\n")
          } else if (i % 9 == 0) {
            result.append("\n")
          } else if (i % 3 == 0) {
            result.append(" ")
          }
        }
        result.append(cell).append(", ")
      }
    }

    static {
      Collection.getMetaClass().mixin(GroovyUtil)
    }
  }
}
