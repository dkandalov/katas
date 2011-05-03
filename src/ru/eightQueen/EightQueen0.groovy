package ru.eightQueen

import org.junit.Test

 /**
 * User: DKandalov
 */
class EightQueen0 { // TODO finish
  @Test
  public void shouldSolve8QueenProblem()
  {
    solve(5).each {println it}
  }

  def solve(int size) {
    def data = []
    (0..size-1).each { data << (0..size-1).collect{0} }

    def solution = []
    (0..size-1).each { row ->

      (0..size-1).each { column ->
        if (isValid(solution + [[row, column]])) {
          solution << [row, column]
        }
      }
    }

    data
  }

  boolean isValid(def solution) {
    boolean sameRowOrCol = solution.any { position ->
      (solution - position).any { position[0] == it[0] || position[1] == position[1] }
    }
    boolean sameDiagonal = solution.any { position ->
      (solution - position).any { Math.abs((int) (position[0] - it[0])) == Math.abs((int) (position[1] - it[1])) }
    }
    !sameRowOrCol && !sameDiagonal
  }

}
