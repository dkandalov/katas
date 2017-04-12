package katas.groovy.fibonacci

import org.junit.Test

/**
 * User: DKandalov
 */
class Fibonacci3_ {
  @Test
  public void shouldCalculateFibonacciNumber() {
      [this.&f, this.&f__, this.&f_, this.&f_nr1, this.&f_nr2].each { fib ->
        assert (0..8).toList().collect{ fib(it) } == [0, 1, 1, 2, 3, 5, 8, 13, 21]
      }
  }

  // recursive process
  private static def f(n) {
    if (n == 0) return 0
    if (n == 1) return 1
    f(n - 1) + f(n - 2)
  }

  // iterative process (recursive procedure)
  private static def f__(lastValue = 1, value = 0, n) {
//    println "$lastValue $value $n"
    if (n == 0) return value
    f__(value, value + lastValue, n - 1)
  }

  // iterative process (recursive procedure)
  private static def f_(value = 0, nextValue = 1, n) {
//    println "$value $nextValue $n"
    if (n == 0) return value
    f_(nextValue, value + nextValue, n - 1)
  }

  // iterative process (iterative procedure)
  private static def f_nr1(n) {
    def (value, nextValue) = [0, 1]
    n.times {
      int tmp = nextValue
      nextValue = value + nextValue
      value = tmp
    }
    value
  }

  // iterative process (iterative procedure)
  private static def f_nr2(n) {
    def (prevValue, value) = [1, 0]
    n.times {
      int tmp = value
      value += prevValue
      prevValue = tmp
    }
    value
  }
}
