package katas.scala.gcd

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 26/10/2011
 */

class GCD2 extends ShouldMatchers {
  @Test def shouldFindGreatestCommonDivider() {
    gcd(1, 1) should equalTo(1)
    gcd(1, 2) should equalTo(1)
    gcd(2, 1) should equalTo(1)

    gcd(2, 2) should equalTo(2)

    for (i <- 1 to 10; j <- 0 to 100 by i) {
      gcd(i, j) should equalTo(i)
      gcd(j, i) should equalTo(i)
    }
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) return a
    gcd(b, a % b)
  }
}