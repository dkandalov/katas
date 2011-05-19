package ru.eightQueen

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * User: DKandalov
 */

class EightQueen1 extends AssertionsForJUnit {

  @Test def shouldFindSolutionFor8QueenProblem() {
    solve(5).foreach{ solution => println(asBoard(5, solution)) }
  }

  type Solution = List[Tuple2[Int, Int]]

  def solve(i: Int): List[Solution] = {
    List(List((1,1), (2,3)))  // TODO finish
  }

  def asBoard(boardSize: Int, solution: Solution): String = {
    val board: Array[Array[Boolean]] = Array.fill(boardSize) { Array.fill(boardSize) { false } }
    solution.foreach{ position =>
      board(position._1)(position._2) = true
    }
    board.foldLeft("") { (result, line) =>
      val lineAsString = line.foldLeft("") { (result, value) => result + (if (value) "1 " else "0 ") }
      result + lineAsString + "\n"
    }
  }
}