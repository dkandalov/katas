package ru.eightQueen

import org.junit.Test

/**
 * User: dima
 * Date: 15/07/2012
 */
class EightQueen5 {
  @Test void shouldFindSolutionsForBoardOfSize_4() {
    def solutions = solveForBoardOfSize(4)
    assert solutions.size == 2
    solutions.each {
      println(asPrintableBoard(it) + "\n")
    }
  }

  def solveForBoardOfSize(int i) {
  }

  static String asPrintableBoard(Object o) {
    ""
  }
}
