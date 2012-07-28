package ru.fibonacci

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 28/07/2012
 */

class Fibonacci6 extends ShouldMatchers {
	@Test def shouldCalculateFibonacciNumbers() {
		fibonacci(0) should equal(1)
		fibonacci(1) should equal(1)
		fibonacci(2) should equal(2)
		fibonacci(3) should equal(3)
		fibonacci(4) should equal(5)
		fibonacci(5) should equal(8)
		fibonacci(6) should equal(13)

		fibonacci(20) should equal(10946)
		fibonacci(100) should equal(1298777728820984005L)
	}

	def fibonacci(n: Int): Long = {
		if (n < 2) return 1

		var prev = 1L
		var result = 1L
		for (i <- 2 to n) {
			val tmp = result
			result = result + prev
			prev = tmp
		}
		result
	}

	def fibonacci_r(n: Int): Int = n match {
		case 0 => 1
		case 1 => 1
		case _ => fibonacci_r(n - 1) + fibonacci_r(n - 2)
	}
}