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

  def List<List> solveForBoardOfSize(int boardSize) {
    doSolve([0, 0], [], boardSize)
  }

  List doSolve(fromQueen, Collection solution, int boardSize) {
    def result = []
    forEachCellOf(boardSize) { row, col ->
      if (row < fromQueen[0] || (row == fromQueen[0] && col < fromQueen[1])) return
      def queen = [row, col]
      if (isValidMove(queen, solution)) {
        doSolve([row, col], solution + queen, boardSize)
      }
    }
    result
  }

  boolean isValidMove(newQueen, Collection solution) {
    def hasNoQueensOnTheSameRowOrColumn = { false }
    def hasNoQueensOnTheDiagonal = { false }
    hasNoQueensOnTheSameRowOrColumn() && hasNoQueensOnTheDiagonal()
  }

  static forEachCellOf(int boardSize, Closure closure) {
    (0..boardSize).each { row ->
      (0..boardSize).each { col ->
        closure.call(row, col)
      }
    }
  }

  static String asPrintableBoard(List solution, int boardSize) {
    def board = (0..boardSize).collect { ("X" * boardSize).toList() }
    solution.each { board[it[0]][it[1]] = "Q" }
    board.join("\n")
  }
}
