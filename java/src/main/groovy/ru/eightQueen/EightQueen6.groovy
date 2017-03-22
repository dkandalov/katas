package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 20/07/2012
 */
@SuppressWarnings("GroovyMissingReturnStatement")
class EightQueen6 {
  @Test void shouldSolveQueenProblem_ForBoardOfSize_3() {
    def solutions = findSolutionsFor(3)
    solutions.each { println asPrintableString(asBoard(it, 3)) + "\n" }
    assert solutions.size() == 0
  }

  @Test void shouldSolveQueenProblem_ForBoardOfSize_4() {
    def solutions = findSolutionsFor(4)
    solutions.each { println asPrintableString(asBoard(it, 4)) + "\n" }
    assert solutions.size() == 2
  }

  @Test void shouldSolveQueenProblem_ForBoardOfSize_5() {
    def solutions = findSolutionsFor(5)
    solutions.each { println asPrintableString(asBoard(it, 5)) + "\n" }
    assert solutions.size() == 10
  }

  @Test void shouldSolveQueenProblem_ForBoardOfSize_8() {
    def solutions = findSolutionsFor(8)
    assert solutions.size() == 92
  }

  def findSolutionsFor(int boardSize) {
    doFindSolutions([0, 0], [], boardSize)
  }

  private def doFindSolutions(fromQueen, solution, boardSize) {
    if (solution.size() == boardSize) return [solution]

    def result = []
    for (int row = 0; row < boardSize; row++) {
      for (int col = 0; col < boardSize; col++) {
        if (row < fromQueen[0] || (row == fromQueen[0] && col < fromQueen[1])) continue
        def newQueen = [row, col]
        if (isValidMove(newQueen, solution)) {
          result += doFindSolutions(newQueen, solution + [newQueen], boardSize)
        }
      }
    }
    result
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
            ["Q", "X", "Q"],
            ["X", "X", "X"],
            ["Q", "X", "Q"]
    ]
    assert asPrintableString(board) == """
Q,X,Q
X,X,X
Q,X,Q
""".trim()
  }

  String asPrintableString(board) {
    board.collect { row -> row.join(",") }.join("\n")
  }

  @Test void shouldPresentSolutionAsABoard() {
    def solution = [[0, 0], [0, 2], [2, 0]]
    assert asBoard(solution, 3) == [
            ["Q", "X", "Q"],
            ["X", "X", "X"],
            ["Q", "X", "X"]
    ]
  }

  def asBoard(solution, int boardSize) {
    def board = (0..<boardSize).collect { (0..<boardSize).inject([]) { board, i -> board + ["X"] } }
    solution.each { queen -> board[queen[0]][queen[1]] = "Q" }
    board
  }
}
