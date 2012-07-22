package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 20/07/2012
 */
@SuppressWarnings("GroovyMissingReturnStatement")
class EightQueen6 {
  @Test public void shouldSolveQueenProblem() {
    findSolutionsFor(4).with {
      assert asBoard(it, 4) == [
              [0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0]
      ]
      assert size() == 2
    }
  }

  def asBoard(solutions, int boardSize) {
    (0..<boardSize).collect { (0..<boardSize).inject([]) { board, i -> board + [0] } }
  }

  def findSolutionsFor(int boardSize) {
    def solution = [[0, 0]]
    [solution]
  }
}
