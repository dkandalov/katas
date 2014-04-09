package ru.backtrack

import org.junit.Test
import org.scalatest.Matchers

/**
 * This code is C translation into scala
 * from Skiena - algo design book; page 232.
 *
 * User: dima
 * Date: 11/04/2012
 */

class Backtrack0 extends Matchers {
  @Test def shouldPrintAllSubsets() {
    println(new Subsets().backtrack(new Array[Int](1000), 0, 3))
  }

  @Test def shouldPrintAllPermutations() {
    println(new Permutations().backtrack(new Array[Int](3), 0, 3))
  }

  class Permutations extends Backtrack {
    val nMax = 10

    override def isASolution(a: Array[Int], k: Int, input: Any) = k == input.asInstanceOf[Int]

    override def processSolution(a: Array[Int], k: Int, input: Any) {
      output.append(a.mkString(",")).append("\n")
    }

    override def constructCandidates(a: Array[Int], k: Int, input: Any): Array[Int] = {
      val n = input.asInstanceOf[Int]

      val inPerm = Array.fill(nMax) { 0 }
      for (i <- 0 until k) inPerm(a(i)) = 1

      var c = Array[Int]()
      for (i <- 1 to n) {
        if (inPerm(i) == 0) { c = c :+ i }
      }
      c
    }
  }

  class Subsets extends Backtrack {

    override def isASolution(a: Array[Int], k: Int, n: Any) = k == n

    override def constructCandidates(a: Array[Int], k: Int, input: Any): Array[Int] = {
      val c = new Array[Int](2)
      c(0) = 1
      c(1) = 0
      c
    }

    override def processSolution(a: Array[Int], k: Int, input: Any) {
      val s = Range(0, k).filter { i => a(i) == 1 }.mkString(",")
      output = output.append(s).append("\n")
    }
  }

  abstract class Backtrack {
    protected var output: StringBuilder = new StringBuilder()

    def backtrack(a: Array[Int], k: Int, input: Any): String = {

      if (isASolution(a, k, input)) {
        processSolution(a, k, input)
      } else {
        val newK = k + 1
        val c = constructCandidates(a, k, input) // in the book it uses "k+1" what doesn't seem to be correct
        for (i <- 0 until c.size) {
          a(k) = c(i)
          makeMove(a, newK, input)
          backtrack(a, newK, input)
          unmakeMove(a, newK, input)
        }
      }
      output.toString()
    }

    def isASolution(a: Array[Int], k: Int, input: Any): Boolean

    def processSolution(a: Array[Int], k: Int, input: Any)

    def constructCandidates(a: Array[Int], k: Int, input: Any): Array[Int]

    def makeMove(a: Array[Int], k: Int, input: Any) {}

    def unmakeMove(a: Array[Int], k: Int, input: Any) {}
  }

}