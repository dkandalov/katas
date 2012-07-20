package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 20/07/2012
 */
class EightQueen6 {
  @Test public void shouldSolveQueenProblem() {
    assert findSoltutionsFor(4).size() == 2
  }

  def findSoltutionsFor(int boardSize) {
    def solution = [[0, 0]]
    [solution]
  }
}
