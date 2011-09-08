package ru.eightQueen

import org.junit.Test

/**
 * @author DKandalov
 */
class EightQueen2 {  // TODO finish
  @Test
  public void shouldFindPositionsWhereQueensDontHitEachOther() {
    assert findSolutions(1) == [[]]
    assert findSolutions(2) == [[]]
    assert findSolutions(3) == [[]]
    assert findSolutions(4) == [[]]
  }

  def findSolutions(int boardSize, int column = 0, def solutions = [[]]) {
    if (column == boardSize) return []
    proposeSolutionsFor(column, boardSize, solutions).each { solution ->
      if (isAcceptable(solution)) {
        solutions += findSolutions(boardSize, column + 1, [solution])
      }
    }
    solutions
  }

  def proposeSolutionsFor(int column, int boardSize, def solutions, Closure closure) {
    [0..boardSize - 1].each { row ->
      solutions.each { solution ->
        closure.call(solution + [[row, column]])
      }
    }
  }

  boolean isAcceptable(def solution) {
    false
  }
}
