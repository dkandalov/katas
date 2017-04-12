package katas.groovy.eightQueen

import org.junit.Test
import katas.java.util.Fail

/**
 * User: dima
 * Date: 02/02/2013
 */
@Fail
class EightQueen8 {
  @Test void aa() {
    assert solveForBoardSize(4) != []
    assert solveForBoardSize(5) != []
  }

  def solveForBoardSize(boardSize, startCol = 0, solution = []) {
    def result = []
    for (row in 0..<boardSize) {
      for (col in startCol..<boardSize) {
        def newSolution = solution + [[row, col]]
        if (isAcceptable(newSolution)) {
          solveForBoardSize(boardSize, col + 1, newSolution).each { result.add(it) }
        }
      }
    }
    result
  }

  boolean isAcceptable(List solution) {
    (solution.every { queen ->
      (solution - queen).every{ it[0] != queen[0] && it[1] != queen[1] }}
            &&
    solution.every { queen ->
      (solution - queen).every{ (it[0] - queen[0]).abs() != (it[1] - queen[1]).abs() }
    })
  }

  def asPrintableBoard(solution) {
    solution.toString()
  }
}
