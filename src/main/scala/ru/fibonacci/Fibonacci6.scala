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
		fibonacci(20) should equal(6765)
		fibonacci(100) should equal(BigDecimal("354224848179261915075"))
		fibonacci(200) should equal(BigDecimal("2.805711729925101400376119324130389E+41"))
		fibonacci(1000) should equal(BigDecimal("4.346655768693745643568852767504065E+208"))
		fibonacci(10000) should equal(BigDecimal("3.364476487643178326662161200510745E+2089"))
	}

	def fibonacci(n: Int): BigDecimal = {
		var state = f(State(n))
		while (state.i < state.n) { state = f(state) }
		state.current
	}

	@Test def shouldProgressFromOneStateToAnother() {
		f(State(0)) should equal(State(0, 0, 0, 0))
		f(State(1)) should equal(State(1, 1, 0, 1))

		f(State(2)) should equal(State(2, 1, 0, 1))
		f(f(State(2))) should equal(State(2, 2, 1, 1))

		f(State(3)) should equal(State(3, 1, 0, 1))
		f(f(State(3))) should equal(State(3, 2, 1, 1))
		f(f(f(State(3)))) should equal(State(3, 3, 1, 2))

		f(State(4)) should equal(State(4, 1, 0, 1))
		f(f(State(4))) should equal(State(4, 2, 1, 1))
		f(f(f(State(4)))) should equal(State(4, 3, 1, 2))
		f(f(f(f(State(4))))) should equal(State(4, 4, 2, 3))
	}

	case class State(n: Int, i: Int = 0, last: BigDecimal = 0, current: BigDecimal = 1)

	def f(state: State): State = {
		if (state.n == 0) return State(0, 0, 0, 0)
		if (state.n == 1) return State(1, 1, 0, 1)

		State(state.n, state.i + 1, state.current, state.last + state.current)
	}

	def fibonacci_iterative(n: Int): BigDecimal = {
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

	def fibonacci_recursive_exponential(n: Int): Int = n match {
		case 0 => 0
		case 1 => 1
		case _ => fibonacci_recursive_exponential(n - 1) + fibonacci_recursive_exponential(n - 2)
	}
}