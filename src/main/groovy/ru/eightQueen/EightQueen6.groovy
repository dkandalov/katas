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
//    if (solution.size() == boardSize) return solution
    for (int row = 0; row < boardSize; row++) {
      for (int col = 0; col < boardSize; col++) {
        if (row < fromQueen[0] || (row == fromQueen[0] && col < fromQueen[1])) continue
        def newQueen = [row, col]
        if (isValidMove(newQueen, solution)) {
          solution += [newQueen]
        }
      }
    }
    [solution]
  }

  @Test public void shouldDetermineIsMoveIsValid() {
    assert isValidMove([0, 0], [])
    assert !isValidMove([0, 0], [[0, 1]])

    assert !isValidMove([0, 0], [[1, 1]])
    assert !isValidMove([5, 7], [[7, 5]])
  }

  boolean isValidMove(newQueen, solution) {
    def queensAreNotOnTheSameRowOrColumn = { !solution.any{ queen -> queen[0] == newQueen[0] || queen[1] == newQueen[1] } }
    def queensAreNotOnTheDiagonal = { !solution.any{ queen -> (queen[0] - newQueen[0]).abs() == (queen[1] - newQueen[1]).abs() } }
    queensAreNotOnTheSameRowOrColumn() && queensAreNotOnTheDiagonal()
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
