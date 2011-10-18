package ru.newton

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
;

/*
 * User: dima
 * Date: 21/2/11
 * Time: 7:11 AM
 */
class Newton0 extends AssertionsForJUnit {
  @Test def shouldFindSquareRootOfNumber() {
    assert(math.abs(sqrt(2) - 1.414) < 0.001, "Actual value: " + sqrt(2))
    assert(math.abs(sqrt(9) - 3) < 0.001)
    assert(math.abs(sqrt(16) - 4) < 0.001)
  }

  def sqrt(n: Double): Double = { // used Int instead of Double
    def sqrtIter(n: Double, guess: Double): Double = {
      if (math.abs(guess * guess - n) < 0.001)
        guess
      else
        sqrtIter(n, (guess + n / guess) / 2)
    }

    sqrtIter(n, 1)
  }
}