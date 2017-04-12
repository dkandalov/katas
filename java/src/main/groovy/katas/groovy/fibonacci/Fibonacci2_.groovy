package katas.groovy.fibonacci

import org.junit.Test

 /**
 * User: DKandalov
 */
class Fibonacci2_ {
  @Test
  public void shouldCalculateFibonacciNumbers() {
    assert fibonacci(0) == 0
    assert fibonacci(1) == 1
    assert fibonacci(2) == 1
    assert fibonacci(3) == 2
    assert fibonacci(4) == 3
    assert fibonacci(5) == 5
    assert fibonacci(6) == 8
    assert fibonacci(7) == 13
    assert fibonacci(8) == 21
  }

  static def fibonacci(int n) {
    if (n == 0) return 0
    if (n == 1) return 1

    int prevValue = 1
    int result = 1
    (n - 2).times {
      int tmp = result
      result += prevValue
      prevValue = tmp
    }

    result
  }

  static def fibonacci_recursive(int n) {
    if (n == 0) return 0
    if (n == 1) return 1
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}
