package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 15/07/2012
 */
class EightQueen5 {
  @Test void shouldFindSolutionsForBoardOfSize_4() {
    def solutions = solveForBoardOfSize(4)
    solutions.each { println(asPrintableBoard(it, 4) + "\n") }
    assert solutions.size() == 2
  }

  @Immutable
  static final class Queen {
    int row
    int col
  }

  static newQueen(row, col) { [row, col] }
  static rowOf(queen) { queen[0] }
  static colOf(queen) { queen[1] }

  def List<List> solveForBoardOfSize(int boardSize) {
    doSolve(newQueen(0, 0), [], boardSize)
  }

  List doSolve(fromQueen, Collection solution, int boardSize) {
    if (solution.size() == boardSize) return [solution]

    def result = []
    for (int row = 0; row < boardSize; row++) {
      for (int col = 0; col < boardSize; col++) {
        if (row < rowOf(fromQueen) || (row == rowOf(fromQueen) && col < colOf(fromQueen))) continue

        def queen = newQueen(row, col)
        if (isValidMove(queen, solution)) {
          result += doSolve(queen, solution + [queen], boardSize)
        }

      }
    }
    result
  }

  static boolean isValidMove(newQueen, Collection solution) {
    def hasNoQueensOnTheSameRowOrColumn = { solution.every { rowOf(it) != rowOf(newQueen) && colOf(it) != colOf(newQueen) } }
    def hasNoQueensOnTheDiagonal = { solution.every { (rowOf(it) - rowOf(newQueen)).abs() != (colOf(it) - colOf(newQueen)).abs() } }
    hasNoQueensOnTheSameRowOrColumn() && hasNoQueensOnTheDiagonal()
  }

  static String asPrintableBoard(List solution, int boardSize) {
    def board = (0..boardSize).collect { ("X" * boardSize).toList() }
    solution.each { board[colOf(it)][rowOf(it)] = "Q" }
    board.join("\n")
  }
}
