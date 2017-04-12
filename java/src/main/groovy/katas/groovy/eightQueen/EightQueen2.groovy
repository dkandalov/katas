package katas.groovy.eightQueen

import org.junit.Test

/**
 * @author DKandalov
 */
class EightQueen2 {
  static X = 1

  @Test
  public void shouldFindPositionsWhereQueensDontHitEachOther() {
    assert findSolutions(1) == [[[0, 0]]]
    assert findSolutions(2) == []
    assert findSolutions(3) == []
    assert findSolutions(4) == [asSolution([
            [0, 0, X, 0],
            [X, 0, 0, 0],
            [0, 0, 0, X],
            [0, X, 0, 0]
    ]), asSolution([
            [0, X, 0, 0],
            [0, 0, 0, X],
            [X, 0, 0, 0],
            [0, 0, X, 0]
    ])]
    assert findSolutions(5).size() == 10
//    assert findSolutions(8).size() == 92 // takes ~8 seconds :(
  }

  static def findSolutions(int boardSize, int column = 0, def solutions = [[]]) {
    if (column == boardSize) return solutions

    def newSolutions = solutions.inject([]) { acc, solution -> acc + proposeNewSolutionsFor(solution, column, boardSize) }
    if (newSolutions.empty) return []
    findSolutions(boardSize, column + 1, newSolutions)
  }

  private static def proposeNewSolutionsFor(def solution, int column, int boardSize) {
    // had wrong syntax for range: was [0..2] instead of (0..2)
    (0..boardSize - 1).collect { row -> solution + [[column, row]] }.findAll { isAcceptable(it) }
  }

  private static boolean isAcceptable(def solution) {
    def onSameColumnOrRow = solution.any { position ->
      (solution - [position]).any { thatPosition -> position[0] == thatPosition[0] || position[1] == thatPosition[1] } // forgot to do (... - position)
    }
    def onSameDiagonal = solution.any { position ->
      (solution - [position]).any { thatPosition -> (position[0] - thatPosition[0]).abs() == (position[1] - thatPosition[1]).abs() }
    }
    !onSameColumnOrRow && !onSameDiagonal
  }

  @Test
  public void shouldDetermineIfPositionOnBoardIsAcceptable() {
    assert isAcceptable(asSolution([
            [0, 0, 0],
            [0, 0, 0],
            [0, 0, 0]
    ]))
    assert isAcceptable(asSolution([
            [X, 0, 0],
            [0, 0, 0],
            [0, 0, 0]
    ]))
    assert !isAcceptable(asSolution([
            [X, 0, 0],
            [0, X, 0],
            [0, 0, 0]
    ]))
    assert !isAcceptable(asSolution([
            [0, 0, 0],
            [0, X, 0],
            [X, 0, 0]
    ]))
    assert !isAcceptable(asSolution([
            [X, 0, X],
            [0, 0, 0],
            [0, 0, 0]
    ]))
    assert !isAcceptable(asSolution([
            [X, 0, 0],
            [0, 0, 0],
            [X, 0, 0]
    ]))
    assert isAcceptable(asSolution([
            [0, 0, X, 0],
            [X, 0, 0, 0],
            [0, 0, 0, X],
            [0, X, 0, 0]
    ]))
  }

  static asSolution(List<List> board) {
    def result = []
    (0..board.size() - 1).each { columnIndex ->
      (0..board.size() - 1).each { rowIndex ->
        if (board[rowIndex][columnIndex] != 0) result << [columnIndex, rowIndex] // used [rowIndex, columnIndex] instead of [rowIndex][columnIndex]
      }
    }
    result
  }
}
