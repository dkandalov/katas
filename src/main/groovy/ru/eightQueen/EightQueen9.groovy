package ru.eightQueen

import org.junit.Test
import ru.util.Fail

/**
 * User: dima
 * Date: 03/02/2013
 */
@Fail
class EightQueen9 {
  @Test void main() {
    println(solveForBoardSize(4).join("\n"))
  }

  def solveForBoardSize(boardSize, col = 0, solution = []) {
    if (col >= boardSize) return [solution]

    def result = []
    for (row in 0..boardSize-1) {
      def newQueen = [col, row]
      if (isAcceptable(solution, newQueen)) {
        solveForBoardSize(boardSize, col + 1, solution + [newQueen]).each{
          result << it
        }
      }
    }
    result
  }

  def isAcceptable(List solution, newQueen) {
    (solution.each{ it[0] != newQueen[0] && it[1] != newQueen[1] }
      &&
    solution.each{ (it[0] - newQueen[0]).abs() != (it[1] - newQueen[1]).abs() })
  }
}
