package katas.scala.eightQueen

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * User: DKandalov
 */
class EightQueen1 extends AssertionsForJUnit {

  @Test def shouldFindSolutionFor8QueenProblem() {
    def boardSize = 8
    solve(boardSize).foreach { solution =>
        println("------------")
        println(solution)
        println(asBoard(boardSize, solution))
    }
  }

  type Solution = List[Tuple2[Int, Int]]

  def solve(n: Int, row: Int = 0): List[Solution] = {
    if (row >= n) return List(List()) // forgot this condition; was List() instead of List(List())

    var solutions: List[Solution] = List() // added List(List()) here
    0.until(n).foreach { column =>
        solve(n, row + 1).foreach { solution => // was "row" instead of "row + 1"
          val newSolution = ((row, column) :: solution)
          if (isValid(newSolution)) { // didn't check newSolution for validness, only returned solution
            solutions = newSolution :: solutions
          }
        }
    }
    solutions
  }

  def isValid(solution: Solution): Boolean = {
    solution.forall { step1 =>
      solution.filter(_ != step1).forall { step2 =>
        (step1._1 != step2._1 && step1._2 != step2._2) && ((step1._1 - step2._1).abs != (step1._2 - step2._2).abs)
      }
    }
  }

  def asBoard(boardSize: Int, solution: Solution): String = {
    val board: Array[Array[Boolean]] = Array.fill(boardSize) { Array.fill(boardSize) { false } }
    solution.foreach { position =>
        board(position._1)(position._2) = true
    }
    board.foldLeft("") { (result, line) =>
        val lineAsString = line.foldLeft("") {
          (result, value) => result + (if (value) "1 " else "0 ")
        }
        result + lineAsString + "\n"
    }
  }
}