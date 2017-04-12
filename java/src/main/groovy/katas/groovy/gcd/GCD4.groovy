package katas.groovy.gcd

import org.junit.Test

/**
 * User: dima
 * Date: 22/11/2011
 */
class GCD4 {
  @Test public void when_given_two_integers_should_find_greatest_common_divider() {
    assert gcd(1, 1) == 1
    assert gcd(2, 1) == 1
    assert gcd(1, 2) == 1
    assert gcd(3, 2) == 1
    assert gcd(2, 3) == 1
    assert gcd(10, 2) == 2
    assert gcd(2, 10) == 2
  }

  static gcd(a, b) {
    if (b == 0) a else gcd(b, a % b)
  }
}
