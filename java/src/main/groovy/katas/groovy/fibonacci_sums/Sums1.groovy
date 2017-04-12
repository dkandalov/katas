package katas.groovy.fibonacci_sums
import org.junit.Test

/**
 * User: dima
 * Date: 22/11/2012
 */
class Sums1 {
  @Test void shouldFindFibonacciRepresentationsOfANumber() {
    assert fibonacciRepresentationsOf(0) == [""]
    assert fibonacciRepresentationsOf(1) == ["1"]
    assert fibonacciRepresentationsOf(2) == ["10"]
    assert fibonacciRepresentationsOf(3) == ["100", "11"]
    assert fibonacciRepresentationsOf(4) == ["101"]
    assert fibonacciRepresentationsOf(5) == ["1000", "110"]
    assert fibonacciRepresentationsOf(6) == ["1001", "111"]
    assert fibonacciRepresentationsOf(7) == ["1010"]
    assert fibonacciRepresentationsOf(8) == ["10000", "1100", "1011"]
    assert fibonacciRepresentationsOf(9) == ["10001", "1101"]
    assert fibonacciRepresentationsOf(10) == ["10010", "1110"]
    assert fibonacciRepresentationsOf(11) == ["10100", "10011", "1111"]
    assert fibonacciRepresentationsOf(12) == ["10101"]
    assert fibonacciRepresentationsOf(13) == ["100000", "11000", "10110"]
  }

  @Test void shouldCalculateFibonacciNumbersSequence() {
    assert calculateFibonacciNumbers(0) == []
    assert calculateFibonacciNumbers(1) == [1]
    assert calculateFibonacciNumbers(2) == [1, 2]
    assert calculateFibonacciNumbers(5) == [1, 2, 3, 5, 8]
    assert calculateFibonacciNumbers(10) == [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
  }

  static List fibs = calculateFibonacciNumbers(100)

  def fibonacciRepresentationsOf(n) {
    f(n, fibs).collect{ asString(it, fibs) }
  }

  private static asString(List numberAsFibonacci, List fibs) {
    if (numberAsFibonacci.empty) return ""

    fibs = fibs.findAll{ it <= numberAsFibonacci.first() }
    println(fibs)
    fibs.reverse().collect{ numberAsFibonacci.contains(it) ? "1" : "0" }.join("")
  }

  private static f(n, List fibs) {
    if (n == 0) return [[]]

    fibs = fibs.findAll{ it <= n }
    if (fibs.empty) return []

    def subResults = f(n - fibs.last(), fibs.take(fibs.size() - 1))
    subResults.collect{ [fibs.last()] + it } + f(n, fibs.take(fibs.size() - 1))
  }

  private static Collection<BigDecimal> calculateFibonacciNumbers(int amount, BigDecimal current = 1, BigDecimal previous = 1) {
    if (amount == 0) []
    else
      [current] + calculateFibonacciNumbers(amount - 1, current + previous, current)
  }
}
