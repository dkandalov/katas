package katas.groovy.fibonacci

import org.junit.Test

/**
 * User: dima
 * Date: 29/1/11
 */
class Fibonacci3 {
  @Test
  public void shouldCalculateFibonacciNumbers() {
    def result = (-1..7).collect {[it, fib(it)]}
    assert result == [
            [-1, -1],
            [0, 0],
            [1, 1],
            [2, 1],
            [3, 2],
            [4, 3],
            [5, 5],
            [6, 8],
            [7, 13]
    ]
  }

  static def fib(int value) {
    if (value < 0) return -1
    if (value == 0) return 0

    int result = 0
    int lastValue = 1
    (1..value).each {
      int tmp = result
      result = result + lastValue
      lastValue = tmp
    }
    result
  }

  static def fib_recursive(int i) {
    if (i < 0) return -1
    if (i < 2) return i
    return fib(i - 2) + fib(i - 1)
  }
}
