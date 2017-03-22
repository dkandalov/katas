package ru.sudoku

import org.junit.Test
import ru.util.GroovyUtil

/**
 * User: dima
 * Date: 28/12/2011
 */
class Sudoku3 {
  @Test public void groovyUnexpectedBehavior() {
    def a = [[row: 0, column: 7]]
    def b = [[row: 1, column: 6]]
    println(a - b) // doesn't behave like removeAll(). I got empty list in groovy 1.8.5, 1.8.7
    a.removeAll(b)
    println a

    Collection.metaClass.mixin(GroovyUtil)
    assert [[row: 0, column: 7]].excluding([[row: 1, column: 6]]) == [[row: 0, column: 7]]
    println([[row: 0, column: 7]] - [[row: 1, column: 6]])
  }

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
      boardOfHints = []
      allCellsByRow().each { cell ->
        if (valueAt(cell) == 0) {
          boardOfHints << (1..9).findAll { canBeUsedAt(cell, it) }
        } else {
          boardOfHints << []
        }
      }
      claimNumbers()
      // TODO pairs, triples, N-sets
      boardOfHints
    }

    def claimNumbers() {
      squaresOfBoard().each { squareCells ->
        rowsInSquare(squareCells).each { rowCells ->
          (1..9).each { value ->
            def valueCanBeInRow = rowCells.any { cell -> hintsAt(cell).contains(value) }
            def notUsedInTheRestOfSquare = !(squareCells.excluding(rowCells)).any { cell -> hintsAt(cell).contains(value) }
            if (valueCanBeInRow && notUsedInTheRestOfSquare) {
              def boardRow = rowCells[0].row
              (cellsInRow(boardRow).excluding(rowCells)).each {
                removeHintAt(it, value)
//                println "removed ${value} at ${it} : ${hintsAt(it)}"
              }
            }
          }
        }
        columnsInSquare(squareCells).each { columnCells ->
          (1..9).each { value ->
            def valueCanBeInColumn = columnCells.any { cell -> hintsAt(cell).contains(value) }
            def notUsedInTheRestOfSquare = !(squareCells.excluding(columnCells)).any { cell -> hintsAt(cell).contains(value) }
            if (valueCanBeInColumn && notUsedInTheRestOfSquare) {
              def boardColumn = columnCells[0].column
              (cellsInColumn(boardColumn).excluding(columnCells)).each {
                removeHintAt(it, value)
//                println "removed ${value} at ${it} : ${hintsAt(it)}"
              }
            }
          }
        }
      }
    }

    @SuppressWarnings("GroovyAssignabilityCheck")
    def rowsInSquare(squareCells) {
      (0..2).collect { row ->
        (0..2).collect { column -> squareCells[row * 3 + column] }
      }
    }

    @SuppressWarnings("GroovyAssignabilityCheck")
    def columnsInSquare(squareCells) {
      (0..2).collect { column ->
        (0..2).collect { row -> squareCells[row * 3 + column] }
      }
    }

    @SuppressWarnings("GroovyAssignabilityCheck")
    def squaresOfBoard() {
      (0..2).collectMany { row ->
        (0..2).collect { column -> cellsInSquare(row, column) }
      }
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
          if (!correct) println "finished in incorrect state :("
          return false
        }
      }
      if (!correct) {
        println "finished in incorrect state :("
        return false
      }
      true
    }

    boolean isCorrect() {
      allCellsByRow().findAll { valueAt(it) != 0 }.every { cell -> canBeUsedAt(cell, valueAt(cell)) }
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

    def removeHintAt(cell, value) {
      boardOfHints[cell.row * 9 + cell.column].removeAll { it == value }
    }

    boolean canBeUsedAt(cell, value) {
      canBeUsedInRow(cell, value) && canBeUsedInColumn(cell, value) && canBeUsedInSquare(cell, value)
    }

    boolean canBeUsedInSquare(cell, value) {
      def sqRow = cell.row.intdiv(3)
      def sqColumn = cell.column.intdiv(3)
      (cellsInSquare(sqRow, sqColumn).excluding(cell)).every { valueAt(it) != value }
    }

    boolean canBeUsedInRow(cell, value) {
      (cellsInRow(cell.row).excluding(cell)).every { valueAt(it) != value }
    }

    boolean canBeUsedInColumn(cell, value) {
      // was cellsInRow instead of cellsInColumn :(
      (cellsInColumn(cell.column).excluding(cell)).every { valueAt(it) != value }
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
