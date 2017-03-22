package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 03/02/2013
 */
class EightQueen9 {
  @Test void shouldFindSolutionsForEightQueenProblem() {
    assert solveForBoardSize(3).size() == 0
    assert solveForBoardSize(4).size() == 2
    assert solveForBoardSize(5).size() == 10
    assert solveForBoardSize(8).size() == 92
  }

  @Test void shouldPrintSolutionInHumanReadableForm() {
    assert asPrintableBoard(solveForBoardSize(4).first()) == """
        --Q-
        Q---
        ---Q
        -Q--
    """.stripIndent().trim()
  }

  def solveForBoardSize(boardSize, col = 0, solution = []) {
    if (col >= boardSize) return [solution]

    def result = []
    for (row in 0..<boardSize) {
      def newQueen = [col, row]
      if (isAcceptable(solution, newQueen)) {
        solveForBoardSize(boardSize, col + 1, solution + [newQueen]).each{ result << it }
      }
    }
    result
  }

  def isAcceptable(List solution, newQueen) {
    (solution.every{ it[0] != newQueen[0] && it[1] != newQueen[1] }
      &&
    solution.every{ (it[0] - newQueen[0]).abs() != (it[1] - newQueen[1]).abs() })
  }

  def asPrintableBoard(solution, boardSize = guessBoardSizeFrom(solution)) {
    def board = (0..<boardSize).collect{ row ->
      (0..<boardSize).inject("") { acc, col ->
        acc + (solution.contains([col, row]) ? "Q" : "-")}
    }
//    solution.each { queen -> board[queen[0]][queen[1]] = "+" }
    board.join("\n")
  }

  def guessBoardSizeFrom(List solution) {
    int maxX = solution.max {it[0]}[0] + 1
    int maxY = solution.max {it[1]}[1] + 1
    Math.max(maxX, maxY)
  }
}
