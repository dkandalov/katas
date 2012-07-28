package ru.fibonacci

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 28/07/2012
 */

class Fibonacci6 extends ShouldMatchers {
	@Test def shouldCalculateFibonacciNumbers() {
		fibonacci(0) should equal(0)
		fibonacci(1) should equal(1)
		fibonacci(2) should equal(1)
		fibonacci(3) should equal(2)
		fibonacci(4) should equal(3)
		fibonacci(5) should equal(5)
		fibonacci(6) should equal(8)
		fibonacci(7) should equal(13)
	}

	@Test def shouldCalculateFibonacci_ForLargeNumbers() {
		fibonacci(20) should equal(10946)
		fibonacci(100) should equal(BigDecimal("573147844013817084101"))
		fibonacci(200) should equal(BigDecimal("4.539736941653079531972969696974110E+41"))
		fibonacci(1000) should equal(BigDecimal("7.033036771142281582183525487718359E+208"))
		fibonacci(10000) should equal(BigDecimal("5.443837311356528133873426099375023E+2089"))
	}

	def fibonacci_(n: Int): BigDecimal = {
		if (n < 2) return 1
		0
	}

	@Test def shouldProgressFromOneStateToAnother() {
		f(State(1, 0, 0, 0)) should equal(State(1, 1, 0, 0))
	}

	case class State(n: Int, i: Int, last: BigDecimal = 1, current: BigDecimal = 1)

	def f(state: State): Option[State] = {
		Some(State(0, 0, BigDecimal(0), BigDecimal(0)))
	}

	def fibonacci(n: Int): BigDecimal = {
		if (n == 0) return 0
		if (n == 1) return 1

		var prev = BigDecimal(0)
		var result = BigDecimal(1)
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