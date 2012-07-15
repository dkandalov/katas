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

  List doSolve(fromQueen, List solution, int boardSize) {
    def result = []
    forEachCellOf(boardSize) { row, col ->

    }
    result
  }

  def forEachCellOf(boardSize, Closure closure) {
    closure.call(0, 0)
  }

  static String asPrintableBoard(List solution, int boardSize) {
    def board = (0..boardSize).collect { ("X" * boardSize).toList() }
    solution.each { board[it[0]][it[1]] = "Q" }
    board.join("\n")
  }
}
