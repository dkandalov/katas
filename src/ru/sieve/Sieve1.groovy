package ru.sieve

import org.junit.Test

/**
 * @author DKandalov
 */
class Sieve1 {
  @Test
  public void shouldFindPrimeNumbers() {
    assert primesUpTo(10) == [2, 3, 5, 7]
    assert primesUpTo(20) == [2, 3, 5, 7, 11, 13, 17, 19]
    assert primesUpTo(100) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
  }

  static primesUpTo(n) {
    if (n < 2) return []
    def primes = primesUpTo(n - 1)
    if (primes.every {n % it != 0}) {
      primes + [n]
    } else {
      primes
    }
  }

  static primesUpTo_imperative(n) {
    def primes = []
    (2..n).each { i -> if (primes.every { i % it != 0}) primes << i }
    primes
  }
}
