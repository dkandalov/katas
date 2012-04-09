package ru.backtrack

import org.junit.Test

/**
 * This code is C translation into scala
 * from Skiena - algo design book; page 232.
 *
 * User: dima
 * Date: 11/04/2012
 */

class Backtrack0 {
  @Test def aaa() {

  }

  class Subsets extends Backtrack {
    def isASolution(a: Array[Int], k: Int, n: Any) = k == n

    def processSolution(a: Array[Int], k: Int, input: Any) {}

    def constructCandidates(a: Array[Int], k: Int, input: Any, c: Array[Int]) {}

    def makeMove(a: Array[Int], k: Int, input: Any) {}

    def unmakeMove(a: Array[Int], k: Int, input: Any) {}
  }

  abstract class Backtrack {
    def backtrack(a: Array[Int], k: Int, input: Any) {
      var c: Array[Int] = new Array(10000)

      if (isASolution(a, k, input)) {
        processSolution(a, k, input)
      } else {
        val newK = k + 1
        constructCandidates(a, newK, input, c)
        for (i <- 0 to c.size) {
          a(k) = c(i)
          makeMove(a, newK, input)
          backtrack(a, newK, input)
          unmakeMove(a, newK, input)
        }
      }
    }

    def isASolution(a: Array[Int], k: Int, input: Any): Boolean

    def processSolution(a: Array[Int], k: Int, input: Any)

    def constructCandidates(a: Array[Int], k: Int, input: Any, c: Array[Int])

    def makeMove(a: Array[Int], k: Int, input: Any)

    def unmakeMove(a: Array[Int], k: Int, input: Any)
  }


}