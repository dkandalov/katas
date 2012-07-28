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
	}

	def fibonacci(n: Int): Int = {
		0
	}
}