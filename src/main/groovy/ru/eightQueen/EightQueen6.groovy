package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 20/07/2012
 */
@SuppressWarnings("GroovyMissingReturnStatement")
class EightQueen6 {
  @Test void shouldSolveQueenProblem_ForBoardOfSize_4() {
    def solutions = findSolutionsFor(4)

    solutions.each { println asPrintableString(asBoard(it, 4)) + "\n" }
    assert solutions.size() == 2
  }

  @Test void shouldSolveQueenProblem_ForBoardOfSize_3() {
    def solutions = findSolutionsFor(3)

    solutions.each { println asPrintableString(asBoard(it, 3)) + "\n" }
    assert solutions.size() == 0
  }

  def findSolutionsFor(int boardSize) {
    doFindSolutions([0, 0], [], boardSize)
  }

  private def doFindSolutions(fromQueen, solution, boardSize) {
    if (solution.size() == boardSize) return solution
    []
  }

  @Test void shouldConvertBoardToPrintableString() {
    def board = [
            [1, 0, 1],
            [0, 0, 0],
            [1, 0, 1],
    ]
    assert asPrintableString(board) == """
[1, 0, 1]
[0, 0, 0]
[1, 0, 1]
""".trim()
  }

  String asPrintableString(board) {
    board.collect { row -> row.toString() }.join("\n")
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
