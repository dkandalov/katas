package katas.groovy.eightQueen

import org.junit.Test
import katas.java.util.Pomodoro
import static org.hamcrest.Matchers.equalTo
import static org.junit.Assert.assertThat

/**
 * User: dima
 * Date: 24/03/2012
 */
@Pomodoro("2")
class EightQueen3 {
  static int boardSize = 4

  @Test public void aaa() {
    def solutions = solve()
    def solutionAsString = solutions.collect {asString(asBoard(it))}.join("\n=======\n")
    assertThat("\n" + solutionAsString + "\n", equalTo("""
0 1 0 0
0 0 0 1
1 0 0 0
0 0 1 0
=======
0 0 1 0
1 0 0 0
0 0 0 1
0 1 0 0
"""))
  }

  def solve(solution = [], initRow = 0) {
    if (solution.size() == boardSize) return [solution]
    def result = []

    for (int row = initRow; row < boardSize; row++) {
      for (int column = 0; column < boardSize; column++) {
        if (solution.contains([column, row])) continue

        def newSolution = solution + [[column, row]]
        if (isCorrect(newSolution)) {
          result.addAll(solve(newSolution, row + 1)) // passed in "column"
        }
      }
    }
    result
  }

  boolean isCorrect(solution) {
    if (solution.any { pos1 -> solution.any{ pos2 ->
      if (pos1 == pos2) false // forgot to exclude itself
      else if (onSameRowOrColumn(pos1, pos2) || onSameDiagonal(pos1, pos2)) true
      else false
    }}) return false
    true
  }

  boolean onSameRowOrColumn(pos1, pos2) {
    return pos1[0] == pos2[0] || pos1[1] == pos2[1]
  }

  boolean onSameDiagonal(pos1, pos2) {
    return (pos1[0] - pos2[0]).abs() == (pos1[1] - pos2[1]).abs()
  }

  static asBoard(solution) {
    def board = (1..boardSize).collect{ (1..boardSize).collect{0} }
    solution.each { position -> board[position[1]][position[0]] = 1 }
    board
  }

  static asString(board) {
    board.collect{ it.join(" ") }.join("\n")
  }
}
