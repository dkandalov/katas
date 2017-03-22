package ru.fibonacci

import org.junit.Test

/**
 * User: dima
 * Date: 2/4/11
 */
class Fibonacci5 {
  @Test public void shouldCalculateFibonacciNumber() {
    assert fib(2) == 1
    assert fib(3) == 2
    assert fib(4) == 3
    assert fib(5) == 5
    assert fib(6) == 8
    assert fib(7) == 13
  }

  def fib0(i) {
    if (i == 0) return 0
    if (i == 1) return 1
    fib(i - 1) + fib(i - 2)
  }

  def fib(i) {
    if (i == 0) return 0
    if (i == 1) return 1

    int result = 1
    int last = 1
    (i - 2).times {
      def tmp = result
      result += last
      last = tmp
    }

    result
  }
}
