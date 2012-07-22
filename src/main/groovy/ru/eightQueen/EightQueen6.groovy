package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 20/07/2012
 */
@SuppressWarnings("GroovyMissingReturnStatement")
class EightQueen6 {
  @Test void shouldSolveQueenProblem() {
    findSolutionsFor(4).with {
      assert size() == 2
    }
  }

  def findSolutionsFor(int boardSize) {
    def solution = [[0, 0]]
    [solution]
  }

  @Test void shouldConvertBoardToPrintableString() {
    def board = [
            [1, 0, 1],
            [0, 0, 0],
            [1, 0, 1],
    ]
    assert asPrintableString(board) == """
[1, 0, 1],
[0, 0, 0],
[1, 0, 1]
""".trim()
  }

  String asPrintableString(List<List<Integer>> board) {
    board.toString()
  }

  @Test void shouldPresentSolutionAsABoard() {
    def solution = [[0, 0], [0, 2], [2, 0]]
    assert asBoard(solution, 3) == [
            [1, 0, 1],
            [0, 0, 0],
            [1, 0, 0]
    ]
  }

  def asBoard(solution, int boardSize) {
    def board = (0..<boardSize).collect { (0..<boardSize).inject([]) { board, i -> board + [0] } }
    solution.each { queen -> board[queen[0]][queen[1]] = 1 }
    board
  }
}
