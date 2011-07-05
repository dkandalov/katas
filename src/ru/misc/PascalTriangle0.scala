package ru.misc

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import collection.immutable.List

/**
 * User: DKandalov
 */

class PascalTriangle0 extends AssertionsForJUnit {
  @Test def aaa() {
    assert(triangle(5) === List(
      List(1),
      List(1, 1),
      List(1, 2, 1),
      List(1, 3, 3, 1),
      List(1, 4, 6, 4, 1)
    ))
  }

  def triangle(depth: Int, input: List[Int] = List(0, 1, 0)): List[List[Int]] = { // couldn't understand whether I need to pass input as a parameter
    def newInput(input: List[Int]): List[Int] = {
      input match { // couldn't match two elements from list correctly (had to resort to println())
        case List(x) => List(0)
        case x1 :: xs => (x1 + xs.head) :: newInput(xs)
      }
    }
    if (depth == 0) return List()
    (input :: triangle(depth - 1, 0 :: newInput(input))).map(_.filter(_ != 0)) // prepended result :: instead of input; forgot to filter out zeroes; filtered zeroes incorrectly
  }
}