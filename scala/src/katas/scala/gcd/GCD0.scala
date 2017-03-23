package katas.scala.gcd

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
;

/*
 * User: dima
 * Date: 21/2/11
 * Time: 7:06 AM
 */
class GCD0 extends AssertionsForJUnit {
  @Test def shouldFindGreatestCommonDivider() {
    assert(gcd(1, 2) == 1)
    assert(gcd(2, 1) == 1)
    assert(gcd(3, 6) == 3)
    assert(gcd(6, 3) == 3)

    assert(gcd(14, 21) == 7)
    assert(gcd(21, 14) == 7)

    assert(gcd(13, 120) == 1)
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}