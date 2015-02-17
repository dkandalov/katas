package ru.eightQueen

import groovy.transform.Immutable
import org.junit.Test

/**
 * User: dima
 * Date: 15/07/2012
 */
class EightQueen5 {
  @Test void shouldFindSolutionsForBoardOfSize_4() {
    def solutions = solveForBoardOfSize(4)
    solutions.each { println(asPrintableBoard(it.coll, 4) + "\n") }
    assert solutions.size() == 2
  }

  @Immutable
  static final class Queen {
    int row
    int col
  }

  static class Solution {
    def coll

    Solution(coll = []) {
      this.coll = coll
    }

    def plus(Queen queen) {
      new Solution(coll + [queen])
    }

    boolean isValidMove(Queen newQueen) {
      def hasNoQueensOnTheSameRowOrColumn = { coll.every { it.row != newQueen.row && it.col != newQueen.col } }
      def hasNoQueensOnTheDiagonal = { coll.every { (it.row - newQueen.row).abs() != (it.col - newQueen.col).abs() } }
      hasNoQueensOnTheSameRowOrColumn() && hasNoQueensOnTheDiagonal()
    }
  }
  
  def List<List> solveForBoardOfSize(int boardSize) {
    doSolve(new Queen(0, 0), new Solution(), boardSize)
  }

  List doSolve(fromQueen, Solution solution, int boardSize) {
    if (solution.coll.size() == boardSize) return [solution]

    def result = []
    for (int row = 0; row < boardSize; row++) {
      for (int col = 0; col < boardSize; col++) {
        if (row < fromQueen.row || (row == fromQueen.row && col < fromQueen.col)) continue

        def queen = new Queen(row, col)
        if (solution.isValidMove(queen)) {
          result += doSolve(queen, solution + queen, boardSize)
        }

      }
    }
    result
  }

  static String asPrintableBoard(List solution, int boardSize) {
    def board = (0..boardSize).collect { ("X" * boardSize).toList() }
    solution.each { board[it.col][it.row] = "Q" }
    board.join("\n")
  }
}
