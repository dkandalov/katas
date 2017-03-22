package ru.gcd

import org.junit.Test

/**
 * User: dima
 * Date: 21/11/2011
 */
class GCD3 {
  @Test public void when_given_two_numbers_should_find_greatest_common_divider() {
    assert gcd(1, 1) == 1
    assert gcd(1, 2) == 1
    assert gcd(2, 1) == 1
    assert gcd(2, 2) == 2

    assert gcd(10, 2) == 2
    assert gcd(2, 10) == 2

    assert gcd(13, 2) == 1
    assert gcd(2, 13) == 1
  }

  static gcd(a, b) {
    while (b != 0) {
      def newB = a % b
      a = b
      b = newB
    }
    a
  }

  static gcd_r(a, b) {
    if (b == 0) a // forgot this case (had case with == 1 before)
    else gcd(b, a % b)
  }
}
