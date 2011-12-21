package ru.sudoku

import org.junit.Test
import ru.util.Mess

/**
 * User: dima
 * Date: 21/12/2011
 */
@Mess
class Sudoku1 {
  @Test public void aaa() {
/*
    def sudoku = new Sudoku([
             0, 0, 6,  8, 0, 0,  3, 9, 0,
             0, 2, 0,  0, 5, 0,  0, 4, 6,
             9, 0, 0,  6, 0, 2,  0, 0, 0,

             0, 0, 0,  0, 7, 0,  4, 8, 0,
             0, 9, 7,  0, 8, 0,  6, 5, 0,
             0, 5, 1,  0, 4, 0,  0, 0, 0,

             0, 0, 0,  1, 0, 5,  0, 0, 4,
             2, 6, 0,  0, 9, 0,  0, 1, 0,
             0, 1, 5,  0, 0, 8,  7, 0, 0
    ])
*/
    // TODO http://www.sudokukingdom.com/ <-- sudoku below is taken from here, cannot solve it
    def sudoku = new Sudoku([
             0, 2, 5,  0, 0, 0,  0, 0, 0,
             0, 0, 0,  2, 8, 0,  3, 5, 0,
             6, 0, 0,  0, 0, 0,  4, 8, 0,

             0, 0, 1,  0, 9, 4,  0, 0, 8,
             0, 0, 0,  5, 0, 0,  0, 0, 0,
             0, 0, 9,  0, 0, 0,  7, 0, 0,

             3, 8, 0,  1, 0, 0,  0, 0, 0,
             0, 0, 0,  0, 0, 0,  9, 6, 1,
             0, 0, 0,  0, 7, 2,  0, 0, 0,
    ])

    while (!sudoku.isSolved()) {
      println sudoku.boardAsString()
      println "========================="
      if (!sudoku.makeStep()) {
        println sudoku.boardAsString()
        println "========================="
        println "Couldn't find any steps"
        return
      }
    }
  }

  private static class Sudoku {
    def board

    Sudoku(def board) {
      this.board = board
    }

    def makeStep() {
      for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
          if (board[row * 9 + col] != 0) continue

          def values = (findMissingInRow(row) + findMissingInCol(col) + findMissingInSquare(row, col)).
                  unique().findAll { value -> canBeUsedIn(row, col, value) }
          if (values.size() == 1) {
            board[row * 9 + col] = values[0]
            println "${row} ; ${col} = ${values[0]}"
            return true
          }
        }
      }

      for (int row = 0; row < 9; row++) {
        for (def value : findMissingInRow(row)) {
          def positions = []

          for (int col = 0; col < 9; col++) {
            if (board[row * 9 + col] != 0) continue
            if (canBeUsedIn(row, col, value)) positions << [row, col]
          }

          if (positions.size() == 1) {
            def col = positions[0][1]
            board[row * 9 + col] = value
            println "${row} ; ${col} = ${value}"
            return true
          }
        }
      }

      for (int col = 0; col < 9; col++) {
        for (def value : findMissingInCol(col)) {
          def positions = []

          for (int row = 0; row < 9; row++) {
            if (board[row * 9 + col] != 0) continue
            if (canBeUsedIn(row, col, value)) positions << [row, col]
          }

          if (positions.size() == 1) {
            def row = positions[0][0]
            board[row * 9 + col] = value
            println "${row} ; ${col} = ${value}"
            return true
          }
        }
      }

      for (int sqRow = 0; sqRow < 9; sqRow += 3) {
        for (int sqCol = 0; sqCol < 9; sqCol += 3) {
          def missingInSquare = (1..9).toList() - (sqRow..sqRow + 2).collect { eachRow ->
            (sqCol..sqCol + 2).collect { eachCol -> board[eachRow * 9 + eachCol] }
          }.flatten()


          for (def value : missingInSquare) {
            def positions =
              (sqRow..sqRow + 2).collect { row ->
                (sqCol..sqCol + 2).findAll { col ->
                  canBeUsedIn(row, col, value)
                }.collect { [row, it] }
              }

//            println value
//            println positions
            if (positions.size() == 1) {
              def row = positions[0][0]
              def col = positions[0][1]
              board[row * 9 + col] = value
              println "${row} ; ${col} = ${value}"
              return true
            }
          }

        }
      }

      false
    }

    def findMissingInSquare(int row, int col) {
      def fromRow = row.intdiv(3) * 3
      def fromCol = col.intdiv(3) * 3
      (1..9).toList() - (fromRow..fromRow + 2).collect { eachRow ->
        (fromCol..fromCol + 2).collect { eachCol -> board[eachRow * 9 + eachCol] }
      }.flatten()
    }

    def findMissingInCol(int col) {
      (1..9).toList() - (0..8).collect { board[it * 9 + col] }
    }

    def findMissingInRow(int row) {
      (1..9).toList() - (0..8).collect { board[row * 9 + it] }
    }

    def canBeUsedIn(int row, int col, def value) {
      def inRow = (0..8).any { board[row * 9 + it] == value}
      def inCol = (0..8).any { board[it * 9 + col] == value}

      def fromRow = row.intdiv(3) * 3
      def fromCol = col.intdiv(3) * 3
      def inSquare = (fromRow..fromRow + 2).any { eachRow ->
        (fromCol..fromCol + 2).any { eachCol ->
          board[eachRow * 9 + eachCol] == value
        }
      }
      !inRow && !inCol && !inSquare
    }

    def isSolved() {
      board.every { it != 0 }
    }

    def boardAsString() {
      def result = ""
      board.eachWithIndex { cell, i ->
        i = i + 1
        if (i % 27 == 0) result += "${cell} \n\n"
        else if (i % 9 == 0) result += "${cell} \n"
        else if (i % 3 == 0) result += "${cell}  "
        else result += "${cell} "
      }
      result
    }
  }
}
