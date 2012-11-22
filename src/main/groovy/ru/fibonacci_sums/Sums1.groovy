package ru.fibonacci_sums
import org.junit.Test

/**
 * User: dima
 * Date: 22/11/2012
 */
class Sums1 {
  @Test void shouldFindFibonacciRepresentationsOfANumber() {
    (0..20).each {
      println(f(it))
    }

//    assert fibonacciRepresentationOf(0) == [""]
//    assert fibonacciRepresentationOf(1) == ["1"]
//    assert fibonacciRepresentationOf(2) == ["10"]
//    assert fibonacciRepresentationOf(3) == ["100", "11"]
  }

  def fibonacciRepresentationOf(n) {
    f(n).collect{asString(it)}
  }

  def asString(List numberAsFibonacci) {
    []
  }

  def f(n, List fibs = calculateFibonacciNumbers(100)) {
    if (n == 0) return [[]]

    fibs = fibs.findAll{ it <= n }
    if (fibs.empty) return []

    def subResults = f(n - fibs.last(), fibs.take(fibs.size() - 1))
    subResults.collect{ [fibs.last()] + it } + f(n, fibs.take(fibs.size() - 1))
  }

  def calculateFibonacciNumbers(amount) {
    [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
  }
}
