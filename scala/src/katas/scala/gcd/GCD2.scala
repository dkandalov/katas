package katas.scala.gcd

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 26/10/2011
 */

class GCD2 extends Matchers {
  @Test def shouldFindGreatestCommonDivider() {
    gcd(1, 1) should equal(1)
    gcd(1, 2) should equal(1)
    gcd(2, 1) should equal(1)

    gcd(2, 2) should equal(2)

    for (i <- 1 to 10; j <- 0 to 100 by i) {
      gcd(i, j) should equal(i)
      gcd(j, i) should equal(i)
    }
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) return a
    gcd(b, a % b)
  }
}