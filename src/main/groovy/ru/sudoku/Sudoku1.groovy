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

    println "result: ${sudoku.solve()}"
  }

  private static class Sudoku {
    static int fails
    def board

    Sudoku(def board) {
      this.board = board
    }

    def solve(int depth = 0) {
//      println "solving:"
//      println boardAsString()
//      println "empty cells: ${board.findAll {it == 0}.size()}"

      def boards = []
      def steps = []

      while (!isSolved() && (steps.empty || (steps.last() != [-1, -1] && steps.last() != [-2, -2]))) {
//        boards << boardAsString()
        def step = makeStep()
        steps << step
      }

//      steps.eachWithIndex { step, i ->
//        println step
//        println "========================="
//        println boards[i]
//        println "========================="
//        if (step == [-1, -1]) {
//          println boards[i]
//        }
//      }

      if (steps.last() == [-2, -2]) {
//        println "No solutions"
        return false
      } else if (steps.last() == [-1, -1]) {
//        println "Couldn't find next step"

        def guesses = []
        for (int row = 0; row < 9; row++) {
          for (int col = 0; col < 9; col++) {
            if (board[row * 9 + col] != 0) continue

            def values = (findMissingInRow(row) + findMissingInCol(col) + findMissingInSquare(row, col)).
                    unique().findAll { value -> canBeUsedIn(row, col, value) }
            guesses << [values.size(), values, row, col]
          }
        }
        guesses = guesses.sort {it[0]}

        for (def guess : guesses) {
          for (def value : guess[1]) {
            def newBoard = board.clone()
            def row = guess[2]
            def col = guess[3]
            newBoard[row * 9 + col] = value
//              println "guessing ${value} at $row : $col"
            if (new Sudoku(newBoard).solve(depth + 1)) {
              return true
            }
          }
        }
//        println "couldn't guess :("
        println "${fails++} @ ${depth}"
        return false
      } else {
        println "solved!!"
        return true
      }
    }

    def makeStep() {
      missingInCol.clear()
      missingInRow.clear()

      for (int row = 0; row < 9; row++) {
        outer1:
        for (def value : findMissingInRow(row)) {
          def position = null

          for (int col = 0; col < 9; col++) {
            if (board[row * 9 + col] != 0) continue
            if (canBeUsedIn(row, col, value)) {
              if (position != null) continue outer1
              position = [row, col]
            }
          }

          if (position != null) {
            def col = position[1]
            board[row * 9 + col] = value
            return position
          } else {
            return [-2, -2]
          }
        }
      }

      for (int col = 0; col < 9; col++) {
        outer2:
        for (def value : findMissingInCol(col)) {
          def position = null

          for (int row = 0; row < 9; row++) {
            if (board[row * 9 + col] != 0) continue
            if (canBeUsedIn(row, col, value)) {
              if (position != null) continue outer2
              position = [row, col]
            }
          }

          if (position != null) {
            def row = position[0]
            board[row * 9 + col] = value
            return position
          } else {
            return [-2, -2]
          }
        }
      }

      for (int sqRow = 0; sqRow < 9; sqRow += 3) {
        for (int sqCol = 0; sqCol < 9; sqCol += 3) {
          def missingInSquare = (1..9).toList() - (sqRow..sqRow + 2).collect { eachRow ->
            (sqCol..sqCol + 2).collect { eachCol -> board[eachRow * 9 + eachCol] }
          }.flatten()


          outer3:
          for (def value : missingInSquare) {
            def position = null

            for (int row = sqRow; row <= sqRow + 2; row++) {
              for (int col = sqCol; col <= sqCol + 2; col++) {
                if (canBeUsedIn(row, col, value)) {
                  if (position == null) continue outer3
                  position = [row, col]
                }
              }
            }

            if (position != null) {
              def row = position[0]
              def col = position[1]
              board[row * 9 + col] = value
              return position
            } else {
              return [-2, -2]
            }
          }
        }
      }

      for (int row = 0; row < 9; row++) {
        outer4:
        for (int col = 0; col < 9; col++) {
          if (board[row * 9 + col] != 0) continue

          def position = null

          def values = (findMissingInRow(row) + findMissingInCol(col) + findMissingInSquare(row, col)).unique()
          for (def value : values) {
            if (canBeUsedIn(row, col, value)) {
              if (position != null) continue outer4
              position = [row, col]
            }
          }

          if (position != null) {
            board[row * 9 + col] = values[0]
            return [row, col]
          } else {
            return [-2, -2]
          }
        }
      }

      [-1, -1]
    }

    def findMissingInSquare(int row, int col) {
      def list = (1..9).toList()

      def values = []
      def fromRow = row.intdiv(3) * 3
      def fromCol = col.intdiv(3) * 3
      for (int eachRow = fromRow; eachRow <= fromRow + 2; eachRow++) {
        for (int eachCol = fromCol; eachCol <= fromCol + 2; eachCol++) {
          values << board[eachRow * 9 + eachCol]
        }
      }

      list.removeAll(values)
      list
    }

    def missingInCol = new HashMap().withDefault {col -> (1..9).toList() - (0..8).collect { board[it * 9 + col] }}
    def findMissingInCol(int col) {
      missingInCol[col]
    }

    def missingInRow = new HashMap().withDefault {row -> (1..9).toList() - (0..8).collect { board[row * 9 + it] }}
    def findMissingInRow(int row) {
      missingInRow[row]
    }

    boolean canBeUsedIn(def row, def col, def value) {
      for (int i = 0; i < 9; i++) {
        if (board[row * 9 + i] == value) return false
        if (board[i * 9 + col] == value) return false
      }

      def fromRow = row.intdiv(3) * 3
      def fromCol = col.intdiv(3) * 3
      for (int i = fromRow; i <= fromRow + 2; i++) {
        for (int j = fromCol; j <= fromCol + 2; j++) {
          if (board[i * 9 + j] == value) return false
        }
      }
      true
    }

    def isSolved() {
      for (def cell: board) {
        if (cell == 0) return false
      }
      true
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
