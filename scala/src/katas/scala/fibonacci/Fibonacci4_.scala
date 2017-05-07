package katas.scala.fibonacci

import org.junit.Test
import org.scalatest.Matchers

import scala.math._

/**
 * @author DKandalov
 */
class Fibonacci4_ extends Matchers {
  @Test def shouldCalculateFibonacciNumber() {
    fib(0) should equal(1)
    fib(1) should equal(1)
    fib(2) should equal(2)
    fib(3) should equal(3)
    fib(4) should equal(5)
    fib(5) should equal(8)

    List[Long](0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(fib) should equal(Seq(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
    List[Long](20, 30, 40, 50, 60, 70, 80, 90).map(fib) should equal(Seq(10946, 1346269, 165580141, 20365011074L, 2504730781961L, 308061521170130L, 37889062373144008L, 4660046610375544832L))
  }

  val fi = (1 + sqrt(5)) / 2

  def fib_(n: Long): Long = {
    floor((pow(fi, n + 1) / sqrt(5)) + 0.5).toLong
  }

  def fib(n: Long): Long = {
    if (n < 0) throw new IllegalArgumentException("n cannot be less than zero")
    var value = 1L
    var lastValue = 1L
    2L.to(n).foreach { i =>
      val tmp = value
      value += lastValue
      lastValue = tmp
    }
    value
  }

  def fib_rec(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException("n cannot be less than zero")
    if (n < 2) return 1
    fib_rec(n - 1) + fib_rec(n - 2)
  }
}